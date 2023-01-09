import { readLines } from "https://deno.land/std@0.171.0/io/mod.ts";

const fileReader = await Deno.open("./01/input.txt");

const elfCalories = [];

let elfIndex = 0;
for await (let line of readLines(fileReader)) {
  if (line.length === 0) {
    elfIndex += 1;
    continue;
  }

  if (elfCalories[elfIndex] === undefined) {
    elfCalories[elfIndex] = [parseInt(line)];
  } else {
    elfCalories[elfIndex].push(parseInt(line));
  }
}

const totals = elfCalories.map((cals) =>
  cals.reduce((total, c) => total + c, 0)
);

// Part One

const highestTotal = totals.reduce(
  (highest, total) => (total > highest ? total : highest),
  0
);

console.log("Highest total calories:", highestTotal);

// Part Two

const highestThreeTotal = totals
  .sort((a, b) => (a < b ? 1 : -1))
  .slice(0, 3)
  .reduce((y, x) => x + y, 0);

console.log(highestThreeTotal);
