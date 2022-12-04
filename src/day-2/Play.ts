import { match, P, Pattern } from 'npm:ts-pattern@^4.0.6'
import * as E from "https://deno.land/x/fp_ts@v2.11.4/Eq.ts";
import * as O from "https://deno.land/x/fp_ts@v2.11.4/Ord.ts";
import { type Ordering } from "https://deno.land/x/fp_ts@v2.11.4/Ordering.ts";

export type Play = '🪨' | '🧻' | '✂️';

export const ordPlay: O.Ord<Play> = O.fromCompare(
  (a, b) => match<[Play, Play], Ordering>([a,b])
    .with(Pattern.union(['🪨', '🪨'], ['🧻', '🧻'], ['✂️', '✂️']), () =>  0)
    .with(Pattern.union(['🪨', '✂️'], ['✂️', '🧻'], ['🧻', '🪨']), () =>  1)
    .with(Pattern.union(['🪨', '🧻'], ['🧻', '✂️'], ['✂️', '🪨']), () => -1)
    .exhaustive()
)

export const chooseMove = (theirs: Play, desiredResult: Ordering) =>
  match<[Play, Ordering], Play>([theirs, desiredResult])
    .with([P.select(), 0], (p) => p)
    .with(['🪨',  1], () => '🧻')
    .with(['🪨', -1], () => '✂️')
    .with(['🧻',  1], () => '✂️')
    .with(['🧻', -1], () => '🪨')
    .with(['✂️',  1], () => '🪨')
    .with(['✂️', -1], () => '🧻')
    .exhaustive()