import * as R from "https://esm.sh/rambda@7.1.4"

let input = Deno.readTextFileSync("./inputs/main")

let inventories = R.pipe(
  R.split("\n\n"),
  R.map(R.split("\n")),
  R.map(R.map(Number)),
)(input)

let partOne = R.pipe(
  R.map(R.sum),
  R.reduce(R.max, -1),
)(inventories)

let partTwo = R.pipe(
  R.map(R.sum),
  R.sort(R.subtract),
  R.reverse<number>,
  R.curry(R.take)(3),
  R.sum,
)(inventories)

console.log(partOne, partTwo)
