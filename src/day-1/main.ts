import * as R from "https://esm.sh/rambda@7.1.4";
import { ElfInventory, sortBySnackWeight, totalCalories } from "./calories.ts";

const input: string = Deno.readTextFileSync("./inputs/main");

const inventories: readonly ElfInventory[] = R.pipe(
  R.split("\n\n"),
  R.map(R.split("\n")),
  R.map(R.map((s) => ({ calories: Number(s) }))),
)(input);

const getLargestInventorySize = R.pipe(
  sortBySnackWeight,
  R.curry(R.take)(1),
  R.map(totalCalories),
);
console.log(R.head(getLargestInventorySize([...inventories])));

const getThreeLargestInventorySizes = R.pipe(
  sortBySnackWeight,
  R.curry(R.take)(3),
  R.map(totalCalories),
);
console.log(R.sum(getThreeLargestInventorySizes([...inventories])));
