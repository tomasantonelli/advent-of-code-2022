import { concat, curry, filter, pipe, repeat, split, splitWhen, startsWith, tail }
from 'https://esm.sh/rambda@7.1.4';
import { List, Stack, } from 'https://deno.land/x/immutable@4.0.0-rc.14-deno/mod.ts';

type EndoFn<T> = (x: T) => T;
const composeT = <T>(...fns: Array<EndoFn<T>>) => fns.reduce((g, f) => ((x) => f(g(x))));
const applyN = <T>(f: EndoFn<T>, n: number) => composeT(...repeat(f, n));

type Crate = string;
type CrateStack = Stack<Crate>;
type Cargo = List<CrateStack>;
type Movement = { readonly amount: number; readonly from: number; readonly to: number; };

const moveCrates = (crateStacks: Cargo, move: Movement, flip = true): Cargo => {
  const fromStack = crateStacks.get(move.from, Stack<Crate>());
  const toStack = crateStacks.get(move.to, Stack<Crate>());

  const [fromStackAfterMovement, cratesRemoved] = applyN<[CrateStack, List<Crate>]>
    (([stack, cratesRemoved]) => {
        return stack.first()
          ? [stack.pop(), cratesRemoved.push(stack.first())]
          : [stack, cratesRemoved];
    }, move.amount)
    ([fromStack, List()]);

  const toStackAfterMovement = flip
    ? toStack.pushAll(cratesRemoved.reverse()) // part one
    : toStack.pushAll(cratesRemoved); // part two

  return crateStacks
    .set(move.from, fromStackAfterMovement)
    .set(move.to, toStackAfterMovement);
};

(() => { // parse parse parse
  const parseCrateLine = (crateStacks: Cargo, line: string): Cargo =>
    crateStacks.map((stack, idx) => {
      const nextCrateposition = 1 + idx * 4;
      const strAtPostition = line[nextCrateposition];
      return strAtPostition.match(/[A-Z]/i)
        ? stack.concat(strAtPostition)
        : stack;
    });

  const parselinesWithCrates = (lines: string[]): Cargo => {
    const numOfStacks = (lines[0].length + 1) / 4;
    const initialStacks: Cargo = List(repeat(Stack(), numOfStacks));
    return lines.reduce(parseCrateLine, initialStacks);
  };

  const parseLineWithMovement = (line: string): Movement => {
    const numbers = line.match(/([0-9]+)/g);
    if (!numbers) throw new Error('Invaid input (movement lines)');
    const [amount, from, to] = numbers.map(Number);
    return { amount: amount, from: from - 1, to: to - 1 };
  };

  const lines: string[] = pipe(
    Deno.readTextFileSync,
    split("\n"),
    filter((line) => line.length > 0),
  )('./inputs/main');

  const [linesWithCrates, rest] = splitWhen(curry(startsWith(" 1")))(lines);

  const initialStacks = parselinesWithCrates(linesWithCrates);
  const movements = tail(rest).map(parseLineWithMovement);

  const partOne = movements
    .reduce((cargo, movement) => moveCrates(cargo, movement), initialStacks)
    .map(stack => stack.first(''))
    .reduce<string>(concat);

  const partTwo = movements
    .reduce((cargo, movement) => moveCrates(cargo, movement, false), initialStacks)
    .map(stack => stack.first(''))
    .reduce<string>(concat);

  console.log('Part One:', partOne);
  console.log('Part Two:', partTwo);
})();
