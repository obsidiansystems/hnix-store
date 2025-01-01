import Criterion (Benchmark)

import Criterion qualified
import Criterion.Main qualified
import Data.Attoparsec.Text.Lazy qualified
import Data.Text.Lazy.IO qualified
import Nix.Derivation qualified

main :: IO ()
main = Criterion.Main.defaultMain benchmarks

benchmarks :: [Benchmark]
benchmarks =
    [ Criterion.Main.env
        (Data.Text.Lazy.IO.readFile "tests/example1.drv")
        bench0
    ]
  where
    bench0 example =
        Criterion.bench "example" (Criterion.nf parseExample example)

    parseExample =
        Data.Attoparsec.Text.Lazy.parse Nix.Derivation.parseDerivation
