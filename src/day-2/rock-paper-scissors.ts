import { match } from 'npm:ts-pattern@^4.0.6'
import { ordPlay, Play } from "./Play.ts";

export type Round = {
  readonly theirs: Play,
  readonly mine: Play
}

const outcomeScore = (r: Round) => match(ordPlay.compare(r.mine, r.theirs))
  .with(-1, () => 0)
  .with( 0, () => 3)
  .with( 1, () => 6)
  .exhaustive()

const shapeScore = (p: Play) => match(p)
  .with('🪨', () => 1)
  .with('🧻', () => 2)
  .with('✂️', () => 3)
  .exhaustive()

export const score = (r: Round): number => outcomeScore(r) + shapeScore(r.mine)