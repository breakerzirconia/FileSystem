import Data.List            (sort)
import FileSystem
import FileSystemEssentials
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "`ls' tests" $ do
    it "Successful" $ do
      fmap sort (runFileSystemMock' ls') `shouldBe` fmap sort (Right ["Dir1", "File1.txt", "Dir2"])
      fmap sort (runFileSystemMock' (ls "ROOT/Dir2")) `shouldBe` fmap sort (Right ["File2.hs"])
    it "Erroneous" $ do
      fmap sort (runFileSystemMock' (ls "ROOT/Dir3")) `shouldBe` Left DirectoryDoesn'tExistException
      fmap sort (runFileSystemMock' (ls "ROOK")) `shouldBe` Left DirectoryDoesn'tExistException
  describe "`cd' tests" $ do
    it "Successful" $ do
      runFileSystemMock' cd' `shouldBe` Right "ROOT"
      runFileSystemMock' (cd "Dir2" >> cd') `shouldBe` Right "ROOT/Dir2"
      runFileSystemMock' (cd "ROOT/Dir1" >> cd') `shouldBe` Right "ROOT/Dir1"
    it "Erroneous" $ do
      runFileSystemMock' (cd "Dir3" >> cd') `shouldBe` Left DirectoryDoesn'tExistException
      runFileSystemMock' (cd "ROOT/Dir1/Dir2" >> cd') `shouldBe` Left DirectoryDoesn'tExistException
  describe "`cat' tests" $ do
    it "Successful" $ do
      runFileSystemMock' (cat "File1.txt") `shouldBe` Right "OMNIDIAMOND"
      runFileSystemMock' (cat "ROOT/Dir2/File2.hs") `shouldBe` Right "module File2 where\n\nf :: Int\nf = 22"
    it "Erroneous" $ do
      runFileSystemMock' (cat "File1.json") `shouldBe` Left FileDoesn'tExistException
      runFileSystemMock' (cat "Dir2/File3.hs") `shouldBe` Left FileDoesn'tExistException
  describe "`touch' tests" $ do
    it "Successful" $ do
      fmap sort (runFileSystemMock' (touch "File3.json" >> ls')) `shouldBe` fmap sort (Right [ "File3.json" 
                                                                                             , "Dir1"
                                                                                             , "File1.txt"
                                                                                             , "Dir2"
                                                                                             ])
      fmap sort (runFileSystemMock' (touch "ROOT/Dir1/YEET" >> cd "Dir1" >> ls')) `shouldBe` fmap sort (Right ["YEET"])
    it "Erroneous" $ do
      runFileSystemMock' (touch "File1.txt") `shouldBe` Left FileAlreadyExistsException
  describe "`mkDir' tests" $ do
    it "Successful" $ do
      fmap length (runFileSystemMock' (mkDir "Dir3" >> ls')) `shouldBe` Right 4
      fmap length (runFileSystemMock' (mkDir "ROOT/Dir1/Dir3" >> cd "Dir1" >> ls')) `shouldBe` Right 1
    it "Erroneous" $ do
      runFileSystemMock' (mkDir "Dir1") `shouldBe` Left DirectoryAlreadyExistsException
  describe "`rmFile' tests" $ do
    it "Successful" $ do
      fmap length (runFileSystemMock' (rmFile "File1.txt" >> ls')) `shouldBe` Right 2
      fmap length (runFileSystemMock' (rmFile "ROOT/Dir2/File2.hs" >> cd "Dir2" >> ls')) `shouldBe` Right 0
    it "Erroneous" $ do
      runFileSystemMock' (rmFile "FileWHATEVER.pdn") `shouldBe` Left FileDoesn'tExistException
  describe "`rmDir' tests" $ do
    it "Successful" $ do
      fmap length (runFileSystemMock' (rmDir "Dir1" >> ls')) `shouldBe` Right 2
    it "Erroneous" $ do
      runFileSystemMock' (rmDir "Dir") `shouldBe` Left DirectoryDoesn'tExistException
  describe "`write' tests" $ do
    it "Successful" $ do
      runFileSystemMock' (write "File1.txt" "0" >> cat "File1.txt") `shouldBe` Right "0"
      runFileSystemMock' (write "ROOT/Dir2/File2.hs" "YEET" >> cat "ROOT/Dir2/File2.hs") `shouldBe` Right "YEET"
    it "Erroneous" $ do
      runFileSystemMock' (write "Fyle.lmao" "DID YA PAY MA DOSH?") `shouldBe` Left FileDoesn'tExistException
  describe "`append' tests" $ do
    it "Successful" $ do
      runFileSystemMock' (append "File1.txt" "0" >> cat "File1.txt") `shouldBe` Right "OMNIDIAMOND0"
      runFileSystemMock' (append "ROOT/Dir2/File2.hs" "?" >> cd "Dir2" >> cat "File2.hs") `shouldBe` Right "module File2 where\n\nf :: Int\nf = 22?"
    it "Erroneous" $ do
      runFileSystemMock' (append "Fyle.lmfao" "IT'S CHUESDAY INNIT") `shouldBe` Left FileDoesn'tExistException
  describe "`size' tests" $ do
    it "Successful" $ do
      runFileSystemMock' (size "File1.txt") `shouldBe` Right 11
      runFileSystemMock' (size "ROOT/Dir2/File2.hs") `shouldBe` Right 38
    it "Erroneous" $ do
      runFileSystemMock' (size "ROOT/HMM") `shouldBe` Left FileDoesn'tExistException
  describe "`permissions' tests" $ do
    it "Successful" $ do
      runFileSystemMock' (permissions "ROOT/File1.txt") `shouldBe` Right [Readable, Writable]
      runFileSystemMock' (permissions "Dir2/File2.hs") `shouldBe` Right [Readable]
    it "Erroneous" $ do
      runFileSystemMock' (permissions "File31.jpeg") `shouldBe` Left FileDoesn'tExistException
  describe "`extension' tests" $ do
    it "Successful" $ do
      runFileSystemMock' (extension "ROOT/File1.txt") `shouldBe` Right ".txt"
      runFileSystemMock' (extension "Dir2/File2.hs") `shouldBe` Right ".hs"
    it "Erroneous" $ do
      runFileSystemMock' (extension "ROOT/OF/ALL/EVIL/JBaited.heh") `shouldBe` Left DirectoryDoesn'tExistException