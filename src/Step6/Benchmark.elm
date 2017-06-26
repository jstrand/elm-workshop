module Step6.Benchmark exposing (..)

import Step6.Step6 exposing (addCard, viewColumn, viewColumn2)
import Benchmark exposing (Benchmark)
import Benchmark.Runner


generatedCards n =
    List.foldr addCard [] (List.repeat n "generated card")


generatedBench name f =
    let
        n =
            1000

        lastId =
            n - 1

        draggedCard =
            Just lastId
    in
        Benchmark.benchmark3 name f (generatedCards n) "Todo" draggedCard


viewColumnBench : Benchmark
viewColumnBench =
    Benchmark.compare ""
        (generatedBench "viewColumn" viewColumn)
        (generatedBench "viewColumn2" viewColumn2)


main =
    Benchmark.Runner.program viewColumnBench
