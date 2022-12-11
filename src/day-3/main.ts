import * as R from "https://esm.sh/rambda@7.1.4";
import { Set } from "https://deno.land/x/immutable@4.0.0-rc.14-deno/mod.ts";

const priority = (item: string): number => {
  const isLowerCase = item.toLowerCase() === item;

  const getPriority = (item: string, baseCharacter: string, offset: number) =>
    item.charCodeAt(0) - baseCharacter.charCodeAt(0) + offset;

  return isLowerCase
    ? getPriority(item, "a", 1)
    : getPriority(item, "A", 27);
};

const commonItem = (rucksacks: string[]): string => {
  const commonItems = rucksacks
    .map(r => Set(r))
    .reduce((found, next) => found.intersect(next));

  if (commonItems.size === 0) throw new Deno.errors.InvalidData;

  return commonItems.first();
};

R.pipe( // Part One
  R.split("\n"),
  R.map((line) => R.splitAt(line.length / 2, line)),
  R.map(commonItem),
  R.map(priority),
  R.sum,
  console.log,
)(Deno.readTextFileSync("./inputs/main"));

R.pipe( // Part Two
  R.split("\n"),
  R.splitEvery(3),
  R.map(commonItem),
  R.map(priority),
  R.sum,
  console.log,
)(Deno.readTextFileSync("./inputs/main"));
