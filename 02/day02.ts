import { readLines } from "https://deno.land/std@0.171.0/io/mod.ts";

const fileReader = await Deno.open("./02/input.txt");

enum Result {
  Draw = 3,
  Lose = 0,
  Win = 6,
}

enum Shape {
  Rock = "ROCK",
  Paper = "PAPER",
  Scissors = "SCISSORS",
}

function outcomePoints(opp: Shape, move: Shape): Result {
  if (opp === Shape.Rock) {
    if (move === Shape.Rock) return Result.Draw;
    if (move === Shape.Paper) return Result.Win;
    if (move === Shape.Scissors) return Result.Lose;
  }
  if (opp === Shape.Paper) {
    if (move === Shape.Rock) return Result.Lose;
    if (move === Shape.Paper) return Result.Draw;
    if (move === Shape.Scissors) return Result.Win;
  }
  if (opp === Shape.Scissors) {
    if (move === Shape.Rock) return Result.Win;
    if (move === Shape.Paper) return Result.Lose;
    if (move === Shape.Scissors) return Result.Draw;
  }
  throw new Error("Invalid moves");
}

function shapePoints(shape: Shape): number {
  switch (shape) {
    case Shape.Rock:
      return 1;
    case Shape.Paper:
      return 2;
    case Shape.Scissors:
      return 3;
    default:
      throw new Error("Invalid shape");
  }
}

function targetShape(result: Result, shape: Shape): Shape {
  switch (result) {
    case Result.Draw:
      return shape;
    case Result.Lose:
      if (shape === Shape.Rock) return Shape.Scissors;
      if (shape === Shape.Paper) return Shape.Rock;
      if (shape === Shape.Scissors) return Shape.Paper;
      break;
    case Result.Win:
      if (shape === Shape.Rock) return Shape.Paper;
      if (shape === Shape.Paper) return Shape.Scissors;
      if (shape === Shape.Scissors) return Shape.Rock;
      break;
  }
  throw new Error("Invalid win shapes");
}

function toResult(code: string): Result {
  switch (code) {
    case "X":
      return Result.Lose;
    case "Y":
      return Result.Draw;
    case "Z":
      return Result.Win;
  }
  throw new Error("Invalid result code");
}

function toShape(code: string): Shape {
  switch (code) {
    case "A":
    case "X":
      return Shape.Rock;
    case "B":
    case "Y":
      return Shape.Paper;
    case "C":
    case "Z":
      return Shape.Scissors;
  }
  throw new Error("Invalid shape code");
}

let score = 0;
let adjustedScore = 0;

for await (let round of readLines(fileReader)) {
  const [oppString, code] = round.split(" ");
  const opp = toShape(oppString);

  // Part One
  const promptedMove = toShape(code);
  score += shapePoints(promptedMove) + outcomePoints(opp, promptedMove);

  // Part Two
  const result = toResult(code);
  const nextMove = targetShape(result, opp);
  adjustedScore += shapePoints(nextMove) + outcomePoints(opp, nextMove);
}

console.log("Total score:", score);
console.log("Adjusted total score:", adjustedScore);
