{-# LANGUAGE ForeignFunctionInterface #-}

module RE where

import System.Plugins.Load
import System.Plugins.Make
import Data.Map
import Data.Maybe
import System.Directory
import System.FileLock
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Foreign
import Foreign.StablePtr
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array

import RETypes

type CallbackFunctionType = Ptr () -> CString -> CInt -> Ptr CString -> IO CInt

foreign import ccall "dynamic" mkCallbackFunctionType :: FunPtr CallbackFunctionType -> CallbackFunctionType
foreign export ccall c_start :: CString -> IO (Ptr ())
foreign export ccall c_stop :: Ptr () -> IO ()
foreign export ccall c_ruleExists :: Ptr () -> CString -> IO CInt
foreign export ccall c_execRule :: Ptr () -> CString -> CInt -> Ptr CString -> Ptr () -> FunPtr CallbackFunctionType -> IO CInt

wrapCallbackFunction :: FunPtr CallbackFunctionType -> Ptr () -> Callback
wrapCallbackFunction cb cbSt fn ps = do
    fnCString <- newCString fn
    psCString <- mapM newCString ps
    psArray <- newArray psCString
    CInt errcode <- mkCallbackFunctionType cb cbSt fnCString (length ps) psArray
    psCString' <- peekArray (length ps) psArray
    ps' <- mapM peekCString psCString'
    mapM free psCString'
    free psArray
    return (fromIntegral errcode, ps')
  
c_start :: CString -> IO (Ptr ())
c_start cobj = do
    obj <- peekCString cobj
    reSt <- hsStart obj
    reStStablePtr <- newStablePtr reSt
    let reStPtr = castStablePtrToPtr reStStablePtr
    return reStPtr

c_stop :: Ptr () -> IO ()
c_stop reStPtr = do
    let reStStablePtr = castPtrToStablePtr reStPtr
    -- reSt <- deRefStablePtr reStStablePtr
    freeStablePtr reStStablePtr
    
c_ruleExists :: Ptr () -> CString -> IO CInt
c_ruleExists reStPtr rnCString = do
    let reStStablePtr = castPtrToStablePtr reStPtr
    reSt <- deRefStablePtr reStStablePtr
    rn <- peekCString rnCString
    ret <- evalStateT (hsRuleExists rn) reSt
    return (CInt (if ret then 1 else 0))

c_execRule :: Ptr () -> CString -> CInt -> Ptr CString -> Ptr () -- callback state
	      -> FunPtr CallbackFunctionType -> IO CInt
c_execRule reStPtr rnCString (CInt n) psArray cbSt cb = do
    let cbHs = wrapCallbackFunction cb cbSt
    let reStStablePtr = castPtrToStablePtr reStPtr
    reSt <- deRefStablePtr reStStablePtr
    rn <- peekCString rnCString
    psCString <- peekArray (fromIntegral n) psArray
    ps <- mapM peekCString psCString
    (errcode, ps') <- evalStateT (hsExecRule rn ps) (cbHs, reSt)
    mapM free psCString
    psCString' <- mapM newCString ps'
    pokeArray psArray psCString'
    return (CInt (fromInteger (toInteger errcode)))
    
remake :: String -> String -> IO Bool
remake objhs objo = do
    hstime <- getModificationTime objhs
    exist <- doesFileExist objo
    if exist 
	then do
	    otime <- getModificationTime objo
	    return (otime <= hstime) 
	else
	    return True
	
hsMake :: String -> IO ()
hsMake obj = do
	let objhs = obj ++ ".hs"
	let objo = obj ++ ".o"
	rem <- remake objhs objo
	if rem 
	    then do
		withFileLock objhs Exclusive (\_ -> do
		    rem <- remake objhs objo
		    if rem 
			then do
			    putStrLn "recompiling"
			    make objhs ["-dynamic", "-fPIC"]
			    return ()
			else
			    return ())
	    else
		return ()
	
	
hsLoad :: String -> IO Module
hsLoad obj = do
	h <- load_ (obj ++ ".o") [] "irods"
	case h of
	    LoadSuccess m _ -> return m
	    LoadFailure msg -> error (show msg)
	    
hsStart :: String -> IO ReState
hsStart obj = do
      hsMake obj
      m <- hsLoad obj
      return (ReState m empty) 
      
hsStop :: ReState -> IO ()
hsStop state = return ()

hsRuleExists :: String -> StateT ReState IO Bool
hsRuleExists rn = do
    ReState m props <- get
    rm <- liftIO (loadFunction m rn)
    return (isJust rm)

hsExecRule :: String -> [ParamType] -> RE (ErrorCode, [ParamType])
hsExecRule rn ps = do
    (_, ReState m props) <- get
    rm <- liftIO (loadFunction m rn)
    case rm of
	 Just r -> (r :: RuleType) ps
	 Nothing -> return (0 - 1, ps)
