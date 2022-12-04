import * as R from "https://esm.sh/rambda@7.1.4";
import { match, Pattern } from "npm:ts-pattern@^4.0.6";
import { fromCompare, Ord } from "https://deno.land/x/fp_ts@v2.11.4/Ord.ts";
import { type Ordering } from "https://deno.land/x/fp_ts@v2.11.4/Ordering.ts";

// Unit type
type Play = "🪨" | "🧻" | "✂️";

// Ord instance
const ordPlay: Ord<Play> = fromCompare(
  (a, b) => match<[Play, Play], Ordering>([a, b])
      .with(Pattern.union(["🪨", "🪨"], ["🧻", "🧻"], ["✂️", "✂️"]), () => 0)
      .with(Pattern.union(["🪨", "✂️"], ["✂️", "🧻"], ["🧻", "🪨"]), () => 1)
      .with(Pattern.union(["🪨", "🧻"], ["🧻", "✂️"], ["✂️", "🪨"]), () => -1)
      .exhaustive(),
);

type Round = { readonly theirs: Play; readonly mine: Play };

const outcomeScore = (r: Round) =>
  match<Ordering, number>(ordPlay.compare(r.mine, r.theirs))
    .with(-1, () => 0)
    .with(0, () => 3)
    .with(1, () => 6).exhaustive();

const shapeScore = (p: Play) => match(p)
  .with("🪨", () => 1)
  .with("🧻", () => 2)
  .with("✂️", () => 3).exhaustive();

const score = (r: Round): number => outcomeScore(r) + shapeScore(r.mine);

// Moar pattern matching
const parsePlay = (s: string) =>
  match<string, Play>(s)
    .with(Pattern.union("A", "X"), () => "🪨")
    .with(Pattern.union("B", "Y"), () => "🧻")
    .with(Pattern.union("C", "Z"), () => "✂️")
    .otherwise((_) => {
      throw new Deno.errors.InvalidData();
    });

const parseLine = (line: string): Round => {
  const words = line.split(/\s/).filter((w) => w.trim().length);
  const [w1, w2, ..._] = words;
  return { theirs: parsePlay(w1), mine: parsePlay(w2) };
};

R.pipe(
  R.split("\n"),
  R.map(parseLine),
  R.map(score),
  R.sum,
  console.log,
)(Deno.readTextFileSync("./inputs/main"));
