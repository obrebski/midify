
import Test.Hspec
import Midify
import Euterpea

main :: IO ()
main = hspec $ do
  describe "Midify tests" $ do

    it "pause 0" $ do
      midify (pause 0) `shouldBe` []

    it "pause 1000/2" $ do
      midify (pause 0) `shouldBe` []

  describe "Euterpea tests" $ do

    it "send (c 4 qn)" $ do
      let (b,v,v') = (_bpm env0, _vel env0, _vel' env0)
          endtime = fromRational (60 / b)
          expected = [(0.0,NoteOn {channel = 0, key = 60, velocity = v}),(endtime,NoteOff {channel = 0, key = 60, velocity = v'})]
      midify (send (c 4 qn)) `shouldBe` expected

    it "send (Modify (Instrument Flute) (rest 0) :: Music Pitch)" $ do
      midify (send (Modify (Instrument Flute) (rest 0) :: Music Pitch))
        `shouldBe` [(0.0,ProgramChange {channel = 0, preset = 73})]
