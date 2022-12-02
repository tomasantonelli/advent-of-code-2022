import * as R from "https://esm.sh/rambda@7.1.4";

export type Snack = { readonly calories: number };

export type ElfInventory = readonly Snack[];

export const totalCalories = (inv: ElfInventory): number =>
  R.pipe(
    R.map<Snack, number>((s) => s.calories),
    R.sum,
  )([...inv]);

export const sortBySnackWeight = (es: ElfInventory[]): ElfInventory[] =>
  R.sort((
    a: ElfInventory,
    b: ElfInventory,
  ) => (((ac: number, bc: number) => ac > bc ? -1 : ac < bc ? 1 : 0)(
    totalCalories(a),
    totalCalories(b),
  )))([...es]);
