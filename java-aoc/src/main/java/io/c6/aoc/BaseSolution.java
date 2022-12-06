package io.c6.aoc;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.Objects;
import java.util.stream.Stream;

public abstract class BaseSolution implements Runnable {

	private static final Logger LOGGER = LoggerFactory.getLogger(BaseSolution.class);

	protected final Year year;
	protected final Day day;

	protected BaseSolution(Year year, Day day) {
		this.year = year;
		this.day = day;
	}

	/**
	 * Overloaded method for `getInput(day, sampleInput)`
	 *
	 * @return stream of input
	 */
	protected Stream<String> getInput() {
		return getInput(false);
	}

	/**
	 * Reusable method to get input for the day's challenge
	 *
	 * @param sampleInput is sample input
	 * @return stream of input
	 */
	protected Stream<String> getInput(final boolean sampleInput) {
		try {
			final var resourceName = String.format(
					"input/%s-day-%s-%s.txt", year.value, day.value, sampleInput ? "sample-input" : "input");
			final var uri = Objects.requireNonNull(getClass().getClassLoader().getResource(resourceName)).toURI();
			return Files.lines(Paths.get(uri));
		} catch (final Throwable e) {
			LOGGER.error("Exception occurred while loading input", e);
		}
		return Stream.empty();
	}

	@Override
	public void run() {
		final var startTime = System.nanoTime();
		solution();
		final var elapsedDuration = Duration.ofNanos(System.nanoTime() - startTime);
		LOGGER.info("Time taken: {} milliseconds", elapsedDuration.toMillis());
	}

	protected abstract void solution();
}
