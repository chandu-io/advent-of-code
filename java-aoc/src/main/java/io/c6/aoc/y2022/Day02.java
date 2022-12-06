package io.c6.aoc.y2022;

import io.c6.aoc.BaseSolution;
import io.c6.aoc.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

import static io.c6.aoc.Day._02;
import static io.c6.aoc.Year._2022;
import static io.c6.aoc.y2022.Day02.Outcome.*;
import static io.c6.aoc.y2022.Day02.Shape.*;

public final class Day02 extends BaseSolution {

	private static final Logger LOGGER = LoggerFactory.getLogger(Day02.class);

	private static final Map<Pair<Shape, Shape>, Outcome> outcomes = Map.of(
			Pair.of(Rock, Rock), Draw,
			Pair.of(Rock, Paper), Win,
			Pair.of(Rock, Scissors), Loose,
			Pair.of(Paper, Rock), Loose,
			Pair.of(Paper, Paper), Draw,
			Pair.of(Paper, Scissors), Win,
			Pair.of(Scissors, Rock), Win,
			Pair.of(Scissors, Paper), Loose,
			Pair.of(Scissors, Scissors), Draw
	);

	private static final Map<Pair<Shape, Outcome>, Shape> myResponses = Map.of(
			Pair.of(Rock, Draw), Rock,
			Pair.of(Rock, Win), Paper,
			Pair.of(Rock, Loose), Scissors,
			Pair.of(Paper, Loose), Rock,
			Pair.of(Paper, Draw), Paper,
			Pair.of(Paper, Win), Scissors,
			Pair.of(Scissors, Win), Rock,
			Pair.of(Scissors, Loose), Paper,
			Pair.of(Scissors, Draw), Scissors
	);

	public Day02() {
		super(_2022, _02);
	}

	public static void main(final String... args) {
		new Day02().run();
	}

	@Override
	public void solution() {
		final var result1 = getInput()
				.map(s -> s.split(" "))
				.map(arr -> Pair.of(OpponentPlay.valueOf(arr[0]), MyResponse.valueOf(arr[1])))
				.map(this::pointsForStrategy1)
				.reduce(0, Integer::sum);
		LOGGER.info("Total score for given first strategy: {}", result1);

		final var result2 = getInput()
				.map(s -> s.split(" "))
				.map(arr -> Pair.of(OpponentPlay.valueOf(arr[0]), MyResponse.valueOf(arr[1])))
				.map(this::pointsForStrategy2)
				.reduce(0, Integer::sum);
		LOGGER.info("Total score for given second strategy: {}", result2);
	}

	private int pointsForStrategy1(final Pair<OpponentPlay, MyResponse> pair) {
		final var shapes = pair.map(l -> l.shape, r -> r.shape);
		return outcomes.get(shapes).points + shapes.right.points;
	}

	private int pointsForStrategy2(final Pair<OpponentPlay, MyResponse> pair) {
		switch (pair.right) {
			case X: return myResponses.get(pair.map(l -> l.shape, r -> Loose)).points + Loose.points;
			case Y: return myResponses.get(pair.map(l -> l.shape, r -> Draw)).points + Draw.points;
			case Z: return myResponses.get(pair.map(l -> l.shape, r -> Win)).points + Win.points;
			default: return Loose.points;
		}
	}

	enum Outcome {
		Win(6), Draw(3), Loose(0);

		final int points;

		Outcome(final int points) {
			this.points = points;
		}
	}

	enum Shape {
		Rock(1), Paper(2), Scissors(3);

		final int points;

		Shape(final int points) {
			this.points = points;
		}
	}

	enum OpponentPlay {
		A(Rock), B(Paper), C(Scissors);

		final Shape shape;

		OpponentPlay(final Shape shape) {
			this.shape = shape;
		}
	}

	enum MyResponse {
		X(Rock), Y(Paper), Z(Scissors);

		final Shape shape;

		MyResponse(final Shape shape) {
			this.shape = shape;
		}
	}
}
