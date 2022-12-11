import { map, pipe, split, filter, length } from "https://esm.sh/rambda@7.1.4";

type Interval = { readonly start: number; readonly end: number };

let contains = (a: Interval, b: Interval) => a.start <= b.start && b.end <= a.end;

let overlap = (a: Interval, b: Interval) =>
  a.start <= b.start && b.start <= a.end ||
  b.start <= a.start && a.start <= b.end;

let intervals = pipe(
  split("\n"),
  map(split(",")),
  map(map(pipe(
    split("-"),
    ([s, e]) => ({ start: Number(s), end: Number(e) }),
  ))),
)(Deno.readTextFileSync("./inputs/main"));

pipe( // Part One
  filter<Interval[]>(([a, b]) => contains(a, b) || contains(b, a)),
  length,
  console.log,
)(intervals);

pipe( // Part Two
  filter<Interval[]>(([a, b]) => overlap(a, b)),
  length,
  console.log,
)(intervals);