import * as R from "https://esm.sh/rambda@7.1.4";
import { match, Pattern } from 'npm:ts-pattern@^4.0.6'

import { Round, score } from './rock-paper-scissors.ts'

import { chooseMove, Play } from "./Play.ts";
import { type Ordering } from "https://deno.land/x/fp_ts@v2.11.4/Ordering.ts";

const input = Deno.readTextFileSync('./inputs/main')

const parsePlayPartOne = (s: string) => match<string, Play>(s)
  .with(Pattern.union('A', 'X'), () => '🪨')
  .with(Pattern.union('B', 'Y'), () => '🧻')
  .with(Pattern.union('C', 'Z'), () => '✂️')
  .otherwise(_ => { throw new Deno.errors.InvalidData })

const parseRoundPartOne = (s: string): Round => {
  const words = s.split(/\s/).filter(w => w.trim().length);
  const [w1, w2, ..._] = words;
  return { theirs: parsePlayPartOne(w1), mine: parsePlayPartOne(w2) }
}

const parseStrategyPlayPartTwo = (s: string) => match<string, Ordering>(s)
  .with('X', () => -1)
  .with('Y', () =>  0)
  .with('Z', () =>  1)
  .otherwise(_ => { throw new Deno.errors.InvalidData })

const parseRoundPartTwo = (s: string): Round => {
  const words = s.split(/\s/).filter(w => w.trim().length);
  const [w1, w2, ..._] = words;

  const theirPlay = parsePlayPartOne(w1);
  const myPlay = chooseMove(theirPlay, parseStrategyPlayPartTwo(w2));

  return { theirs: theirPlay, mine: myPlay }
}

const partOne = R.pipe(
  R.split('\n'),
  R.filter(line => line.trim().length > 0),
  R.map(parseRoundPartOne),
  R.map(score),
  R.sum
)(input)

console.log(partOne)

const partTwo = R.pipe(
  R.split('\n'),
  R.filter(line => line.trim().length > 0),
  R.map(parseRoundPartTwo),
  R.map(score),
  R.sum
)(input)

console.log(partTwo)
