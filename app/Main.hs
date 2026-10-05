{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- Aeson convierte entre los tipos de Haskell y el JSON que envia/recibe Firestore.
import Data.Aeson (ToJSON, FromJSON(parseJSON), encode, decode, withObject, (.:), (.:?))
import GHC.Generics (Generic)
import Network.HTTP.Conduit (simpleHttp, parseRequest, method, requestBody, RequestBody(RequestBodyLBS), httpLbs)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Maybe as Maybe
import System.IO (hFlush, stdout)

-------------------------------------------------------------------------------
-- CONFIGURACIÓN Y MODELOS PARA LA API REST DE CLOUD FIRESTORE
-------------------------------------------------------------------------------

-- Ruta REST de la coleccion. El ID del proyecto y el nombre de la coleccion
-- deben coincidir con los configurados en Firebase.
-- Esta URL no incluye autenticacion: el acceso depende de las reglas de
-- seguridad de Firestore. En produccion se debe usar autenticacion segura;
-- nunca se debe publicar una clave privada de una cuenta de servicio.
firebaseURL :: String
firebaseURL = "https://firestore.googleapis.com/v1/projects/gestion-usuarios-d40a6/databases/(default)/documents/usuario"

-- Firestore representa cada valor con un tipo JSON explicito, por ejemplo:
-- {"stringValue":"Ana"}. Este tipo refleja esa envoltura.
newtype StringValue = StringValue { stringValue :: String }
    deriving (Show, Generic)

-- Los nombres de estos campos deben coincidir con las llaves guardadas en
-- "fields" dentro de cada documento de Firestore.
data Campos = Campos
    { nombre     :: StringValue
    , apellido   :: StringValue
    , correo     :: StringValue
    -- Solo para demostracion: este ejemplo guarda la contrasena como texto.
    -- En una aplicacion real debe usarse Firebase Authentication, no almacenar
    -- contrasenas directamente en los documentos.
    , contrasena :: StringValue
    , rol        :: StringValue
    , idPersona  :: StringValue
    } deriving (Show, Generic)

data DocumentoFirestore = DocumentoFirestore
    { fields :: Campos
    } deriving (Show, Generic)

-- Generic permite que Aeson genere la conversion a JSON a partir de los tipos.
-- Al escribir, el documento queda con la forma {"fields":{...}} que espera
-- la API REST de Firestore.
instance ToJSON StringValue
instance ToJSON Campos
instance ToJSON DocumentoFirestore

-- La lectura de StringValue exige la llave "stringValue". El operador (.:)
-- usado abajo tambien exige cada campo; si falta alguno, decode falla.
instance FromJSON StringValue where
    parseJSON = withObject "StringValue" $ \v -> StringValue <$> v .: "stringValue"

instance FromJSON Campos where
    parseJSON = withObject "Campos" $ \v -> Campos
        <$> v .: "nombre"
        <*> v .: "apellido"
        <*> v .: "correo"
        <*> v .: "contrasena"
        <*> v .: "rol"
        <*> v .: "idPersona"

data DocumentoRespuesta = DocumentoRespuesta
    { fieldsRes :: Maybe Campos
    } deriving (Show, Generic)

-- Firestore puede omitir "fields" en una respuesta; (.:?) lo representa como
-- Nothing en vez de producir un error de parseo.
instance FromJSON DocumentoRespuesta where
    parseJSON = withObject "DocumentoRespuesta" $ \v -> DocumentoRespuesta <$> v .:? "fields"

data ListaDocumentos = ListaDocumentos
    { documents :: Maybe [DocumentoRespuesta]
    } deriving (Show, Generic)

-- La API puede omitir "documents", por ejemplo cuando la coleccion esta vacia.
instance FromJSON ListaDocumentos where
    parseJSON = withObject "ListaDocumentos" $ \v -> ListaDocumentos <$> v .:? "documents"

-------------------------------------------------------------------------------
-- FUNCIONES AUXILIARES DE RED
-------------------------------------------------------------------------------

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

-- Envia una peticion HTTP con el metodo y cuerpo indicados. TLS cifra el
-- transporte. Esta version no agrega token de autenticacion ni inspecciona el
-- codigo HTTP: devuelve 0 si httpLbs termina sin lanzar una excepcion. Por eso
-- el valor usado por las operaciones CRUD no confirma por si solo que Firestore
-- haya aceptado la peticion; algunos errores HTTP pueden lanzar una excepcion.
enviarPeticion :: String -> String -> LBS.ByteString -> IO Int
enviarPeticion url tipoMetodo cuerpo = do
    manager <- newManager tlsManagerSettings
    reqInicial <- parseRequest url
    let request = reqInicial
            { method = B8.pack tipoMetodo
            , requestBody = RequestBodyLBS cuerpo
            }
    _ <- httpLbs request manager
    return 0

-------------------------------------------------------------------------------
-- OPERACIONES CRUD (FIRESTORE)
-------------------------------------------------------------------------------

-- 1. AGREGAR USUARIO (CREATE): POST crea un documento. documentId fija el ID
-- del documento con el identificador ingresado por el usuario.
agregarUsuario :: IO ()
agregarUsuario = do
    putStrLn "\n--- Registro de Nuevo Usuario en Cloud Firestore ---"
    idp  <- prompt "ID Persona (Código único alfanumérico): "
    nom  <- prompt "Nombre: "
    ape  <- prompt "Apellido: "
    corr <- prompt "Correo Electrónico: "
    pass <- prompt "Contraseña: "
    uRol <- prompt "Rol (USER / ADMIN): "

    let userDoc = DocumentoFirestore (Campos (StringValue nom) (StringValue ape) (StringValue corr) (StringValue pass) (StringValue uRol) (StringValue idp))
        url = firebaseURL ++ "?documentId=" ++ idp
        cuerpo = encode userDoc

    res <- enviarPeticion url "POST" cuerpo
    if res == 0
        then putStrLn "Usuario insertado correctamente en Firestore."
        else putStrLn "Error al insertar el registro."

-- 2. LISTAR USUARIOS (READ): GET devuelve los documentos como JSON; decode
-- intenta convertir esa respuesta al modelo ListaDocumentos.
listarUsuarios :: IO ()
listarUsuarios = do
    putStrLn "\n--- DATOS EN CLOUD FIRESTORE ---"
    jsonRespuesta <- simpleHttp firebaseURL

    case (decode jsonRespuesta :: Maybe ListaDocumentos) of
        Nothing -> putStrLn "Error: Estructura JSON incompatible con el modelo de Haskell."
        Just (ListaDocumentos Nothing) -> putStrLn "La colección 'usuario' está vacía."
        Just (ListaDocumentos (Just docs)) -> do
            -- Ignora documentos sin "fields" para detectar si hay datos que
            -- puedan convertirse al modelo Campos.
            let documentosValidos = filter (Maybe.isJust . fieldsRes) docs
            if null documentosValidos
                then putStrLn "Se encontraron documentos, pero las llaves internas no corresponden a tu modelo."
                else mapM_ imprimirDoc docs
  where
    imprimirDoc doc = case fieldsRes doc of
        Nothing -> return ()
        Just c  -> putStrLn $ "ID: " ++ stringValue (idPersona c)
                           ++ " | " ++ stringValue (nombre c) ++ " " ++ stringValue (apellido c)
                           ++ " | Correo: " ++ stringValue (correo c)
                           ++ " | Rol: " ++ stringValue (rol c)

-- 3. ACTUALIZAR USUARIO (UPDATE): PATCH envia los campos del documento
-- identificado por idp. La API requiere que ese documento ya exista.
actualizarUsuario :: IO ()
actualizarUsuario = do
    putStrLn "\n--- Actualizar Información de Usuario ---"
    idp  <- prompt "Ingrese el ID Persona del usuario a modificar: "
    nom  <- prompt "Nuevo Nombre: "
    ape  <- prompt "Nuevo Apellido: "
    corr <- prompt "Nuevo Correo: "
    pass <- prompt "Nueva Contraseña: "
    uRol <- prompt "Nuevo Rol: "

    let userDoc = DocumentoFirestore (Campos (StringValue nom) (StringValue ape) (StringValue corr) (StringValue pass) (StringValue uRol) (StringValue idp))
        url = firebaseURL ++ "/" ++ idp
        cuerpo = encode userDoc

    res <- enviarPeticion url "PATCH" cuerpo
    if res == 0
        then putStrLn "Documento modificado correctamente en Firestore."
        else putStrLn "Error al intentar actualizar."

-- 4. ELIMINAR USUARIO (DELETE): solicita borrar el documento identificado por
-- idp; la operacion es permanente si Firestore acepta la peticion.
eliminarUsuario :: IO ()
eliminarUsuario = do
    putStrLn "\n--- Eliminar Usuario de Firestore ---"
    idp <- prompt "Ingrese el ID Persona del usuario que desea eliminar: "

    let url = firebaseURL ++ "/" ++ idp
    res <- enviarPeticion url "DELETE" LBS.empty
    if res == 0
        then putStrLn "Usuario eliminado de forma definitiva."
        else putStrLn "Error al intentar eliminar el registro."

-------------------------------------------------------------------------------
-- MENÚ INTERACTIVO
-------------------------------------------------------------------------------

ejecutarMenu :: IO ()
ejecutarMenu = do
    putStrLn "\n**************************************"
    putStrLn "  SISTEMA DE GESTION HASKELL-FIRESTORE"
    putStrLn "**************************************"
    putStrLn "1 - Agregar nuevo usuario"
    putStrLn "2 - Listar usuarios"
    putStrLn "3 - Actualizar usuario"
    putStrLn "4 - Eliminar usuario"
    putStrLn "5 - Salir"
    opcion <- prompt "Seleccione una opcion: "

    -- Cada operacion vuelve a mostrar el menu; la opcion 5 termina la recursion.
    case opcion of
        "1" -> agregarUsuario >> ejecutarMenu
        "2" -> listarUsuarios >> ejecutarMenu
        "3" -> actualizarUsuario >> ejecutarMenu
        "4" -> eliminarUsuario >> ejecutarMenu
        "5" -> putStrLn "Cerrando aplicación..."
        _   -> putStrLn "Opcion no valida, intente de nuevo." >> ejecutarMenu

main :: IO ()
main = do
    -- Punto de entrada del programa: inicia el menu y espera que este finalice.
    ejecutarMenu
    putStrLn "Sistema finalizado."
