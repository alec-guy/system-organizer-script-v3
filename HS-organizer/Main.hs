{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle.Prelude (lstree, grepText, testfile, mkdir , cp)
import Turtle (liftIO, extension, hasExtension, filename)
import Turtle.Shell (sh, reduce)
import Data.Set as Set 
import Data.Text as Text 
import qualified Data.Text.IO as Textio
import qualified Control.Foldl as Foldl (list)
import Control.Monad (void, sequence_, sequence)
import qualified Data.Map.Strict as SM
import System.IO (writeFile)
import Data.Maybe (fromJust)
import Data.List (partition, (\\))
import Data.Char
import Control.Exception (try, SomeException)

-- Objective One  : Finds all files in a directory and its subdirectories.
-- Objective Two  : sorts these files into folders based on their file extensions. 
-- Objective Three: Calculates and prints the total size of all files for each type. 


main :: IO ()
main = do
    let iotexts = reduce Foldl.list $ do 
                          fp <- lstree "."
                          let textFP = Text.pack fp
                          pure $ textFP
    texts <- iotexts 
    let text          = (Text.concat texts)
        splitPaths    :: Set Text
        splitPaths    = 
            Set.map (\t -> ".\\" `Text.append` t) (Set.fromList $ Text.splitOn (Text.pack ".\\") text)  

        makeFile :: Int -> Set Text -> IO (Maybe Text)
        makeFile i st = do 
            let maybeFile = elemAt i st 
            isFile <- testfile (Text.unpack maybeFile)
            case isFile of 
                True -> pure $ Just maybeFile
                _    -> pure Nothing 
        
        pathsToFiles :: Int -> Set Text -> [IO (Maybe Text)]
        pathsToFiles i st = 
            case Set.null st of 
                True  -> [] 
                False -> case i >= (size st) of 
                          True  -> [] 
                          False -> (makeFile i st) : (pathsToFiles (i + 1) st)

        sequencePathsToFiles :: IO [Maybe Text]
        sequencePathsToFiles = sequence $ pathsToFiles 0 splitPaths

    maybeFiles <- sequencePathsToFiles

    let justFiles  = Prelude.filter (/= Nothing)  maybeFiles 
        files      = Prelude.map fromJust justFiles 

        makeFolder :: Text -> Folder 
        makeFolder txt = Folder (Set.singleton txt)

        makeFolders :: [Text] -> [Folder]
        makeFolders folders = 
            case folders of 
                []       -> [] 
                (f : fs) -> 
                    case (Data.List.partition (hasSameExtension f) fs) of 
                        (does, doesNot)  ->  
                            case does of 
                                [] -> 
                                 (Folder (Set.singleton f))   : (makeFolders doesNot)
                                _  -> 
                                 (Folder ((Set.singleton f) `union` (Set.fromList does))) : (makeFolders (doesNot Data.List.\\ does)) 
        folders :: [Folder]
        folders = makeFolders files
    physicallyMakeFolders folders
    pure ()

        
physicallyMakeFolder :: Folder -> IO ()
physicallyMakeFolder folder = do 
          mkdir (makeFolderName folder)
          case folder of 
            (Folder set) -> do
                let files   = Set.toList set
                    newPath :: Text -> String
                    newPath oldpath = ".//" ++ (makeFolderName folder) ++ "/" ++ (filename $ Text.unpack oldpath)
                sequence_  $ (\oldPath -> void $ (try (cp (Text.unpack oldPath) (newPath oldPath)) :: IO (Either SomeException ()))) <$> files 
                            
physicallyMakeFolders :: [Folder] -> IO ()
physicallyMakeFolders folders = 
    Prelude.foldr (\folder acc -> void (try (physicallyMakeFolder folder) :: IO (Either SomeException ())) >> acc) (pure ()) folders

makeFolderName :: Folder -> String 
makeFolderName folder = 
    case folder of 
        (Folder set) -> case elemAt 0 set of 
                         elem -> (Data.Char.toUpper <$> extension' elem) ++ ("-organizer") 


newtype Folder = Folder (Set Text) deriving (Show , Eq) 
----------
hasSameExtension :: Text -> Text -> Bool 
hasSameExtension f f2 = (extension' f) == (extension' f2)

extension' :: Text -> String 
extension' file = 
    let filepath = Text.unpack file 
    in case extension filepath of 
        Nothing   -> filepath
        (Just ex) -> ex
------------

    