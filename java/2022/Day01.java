import java.io.*;
import java.math.*;
import java.nio.*;
import java.nio.channels.*;
import java.nio.charset.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.*;
import java.util.regex.*;
import java.util.stream.*;

public class Day01 {

//	private static final String INPUT_PATH = "../../__input__/2022/day-01-input-sample.txt";
	private static final String INPUT_PATH = "../../__input__/2022/day-01-input.txt";

	public static void main(final String... args) {
		Triplet triplet = mostCalories(getInput());
		log("Top 3 Elves carrying most calories    : ", triplet);
		log("Elf carrying most calories            : ", triplet.max());
		log("Total calories carried by top 3 Elves : ", triplet.sum());
	}

	private static Triplet mostCalories(final Stream<String> input) {
		var acc = 0;
		var triplet = new Triplet();
		for (final var s : input.collect(Collectors.toUnmodifiableList())) {
			if (s.equals("")) {
				triplet = triplet.replaceIfBigger(acc);
				acc = 0;
			} else {
				final var n = Integer.parseInt(s);
				acc += n;
			}
		}
		triplet = triplet.replaceIfBigger(acc);
		return triplet;
	}

	private static final class Triplet {

		public final int a;
		public final int b;
		public final int c;

		public Triplet() {
			this(new int[0]);
		}

		public Triplet(final int... values) {
			final var n = values == null ? 0 : values.length;
			if (n == 0) { a = b = c = 0; }
			else if (n == 1) { a = values[0]; b = c = 0; }
			else if (n == 2) { a = values[0]; b = values[1]; c = 0; }
			else { a = values[0]; b = values[1]; c = values[2]; }
		}

		public Triplet replaceIfBigger(final int d) {
			final var m = min();
			if (d > m) {
				if (m == a) { return new Triplet(d, b, c); }
				else if (m == b) { return new Triplet(a, d, c); }
				else if (m == c) { return new Triplet(a, b, d); }
				else { return this; }
			} else {
				return this;
			}
		}

		public int max() { return Math.max(a, Math.max(b, c)); }

		public int min() { return Math.min(a, Math.min(b, c)); }

		public int sum() { return a + b + c; }

		@Override
		public String toString() {
			return a + ", " + b + ", " + c;
		}
	}

	// Reusable method to get input for the day's challenge
	private static Stream<String> getInput() {
		try {
			return Files.lines(Paths.get(INPUT_PATH));
		} catch (final Throwable e) {
			log("Exception occurred: ", e.getMessage());
		}
		return Stream.empty();
	}

	// Utility logger
	private static void log(final Object... args) {
		System.out.println(Stream.of(args).reduce("", (a, b) -> a + "" + b));
	}
}
