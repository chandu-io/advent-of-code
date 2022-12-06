package io.c6.aoc.y2022;

import io.c6.aoc.BaseSolution;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static io.c6.aoc.Day._02;
import static io.c6.aoc.Year._2022;

public final class Day02 extends BaseSolution {

	private static final Logger LOGGER = LoggerFactory.getLogger(Day02.class);

	public Day02() {
		super(_2022, _02);
	}

	public static void main(final String... args) {
		new Day02().run();
	}

	@Override
	public void solution() {
		getInput().forEach(LOGGER::info);
	}
}
