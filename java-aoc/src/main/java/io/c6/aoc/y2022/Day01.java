package io.c6.aoc.y2022;

import io.c6.aoc.BaseSolution;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.c6.aoc.Day._01;
import static io.c6.aoc.Year._2022;

public final class Day01 extends BaseSolution {

	private static final Logger LOGGER = LoggerFactory.getLogger(Day01.class);

	public Day01() {
		super(_2022, _01);
	}

	public static void main(final String... args) {
		new Day01().run();
	}

	@Override
	public void solution() {
		final var input = getInput();
		final var triplet = mostCalories(input);
		LOGGER.info("Top 3 Elves carrying most calories    : {}", triplet);
		LOGGER.info("Elf carrying most calories            : {}", triplet.max());
		LOGGER.info("Total calories carried by top 3 Elves : {}", triplet.sum());
	}

	/**
	 * Implementation 1: Using additional data structure `TripletWithRunningSum`.
	 *
	 * @param input stream of inputs
	 * @return a triplet containing top 3 calories
	 */
	private Triplet mostCalories(final Stream<String> input) {
		final var tripletWithRunningSum = input.reduce(
				new TripletWithRunningSum(),
				(t, s) -> s.equals("") ? t.foldSum() : t.addToSum(Integer.parseInt(s)),
				TripletWithRunningSum::combine);
		tripletWithRunningSum.foldSum();
		return tripletWithRunningSum.triplet;
	}

	/**
	 * Implementation 1: Using atomic data structures. Better that implementation 3 for large inputs.
	 *
	 * @param input stream of inputs
	 * @return a triplet containing top 3 calories
	 */
	private Triplet mostCaloriesWithAtomicReferences(final Stream<String> input) {
		final var accRef = new AtomicInteger(0);
		final var tripletRef = new AtomicReference<>(new Triplet());
		input.forEach(s -> accRef.getAndUpdate(acc -> {
			if (s.equals("")) {
				tripletRef.getAndUpdate(triplet -> triplet.replaceIfBigger(acc));
				return 0;
			} else {
				return acc + Integer.parseInt(s);
			}
		}));
		tripletRef.getAndUpdate(triplet -> triplet.replaceIfBigger(accRef.get()));
		return tripletRef.get();
	}

	/**
	 * Implementation 3: Using plain mutation and for-loop. Not ideal for large inputs.
	 *
	 * @param input stream of inputs
	 * @return a triplet containing top 3 calories
	 */
	private Triplet mostCaloriesWithMutation(final Stream<String> input) {
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

	/**
	 * A data structure to hold three integer values,
	 * with option to replace the lowest with a value bigger than the lowest.
	 */
	private static final class Triplet {

		public final int a;
		public final int b;
		public final int c;

		public Triplet() {
			this(new int[0]);
		}

		public Triplet(final int... values) {
			final var n = values == null ? 0 : values.length;
			if (n == 0) {
				a = b = c = 0;
			} else if (n == 1) {
				a = values[0];
				b = c = 0;
			} else if (n == 2) {
				a = values[0];
				b = values[1];
				c = 0;
			} else {
				a = values[0];
				b = values[1];
				c = values[2];
			}
		}

		public Triplet replaceIfBigger(final int d) {
			final var m = min();
			if (d > m) {
				if (m == a) {
					return new Triplet(d, b, c);
				} else if (m == b) {
					return new Triplet(a, d, c);
				} else if (m == c) {
					return new Triplet(a, b, d);
				} else {
					return this;
				}
			} else {
				return this;
			}
		}

		public static Triplet combine(final Triplet t1, final Triplet t2) {
			return t1.replaceIfBigger(t2.a).replaceIfBigger(t2.b).replaceIfBigger(t2.c);
		}

		public int max() {
			return Math.max(a, Math.max(b, c));
		}

		public int min() {
			return Math.min(a, Math.min(b, c));
		}

		public int sum() {
			return a + b + c;
		}

		@Override
		public String toString() {
			return a + ", " + b + ", " + c;
		}
	}

	/**
	 * A data structure to hold triplet and running sum for a stream of input data.
	 */
	private static final class TripletWithRunningSum {
		public final Triplet triplet;
		public final int sum;

		public TripletWithRunningSum() {
			this(new Triplet(), 0);
		}

		public TripletWithRunningSum(final Triplet triplet) {
			this(triplet, 0);
		}

		public TripletWithRunningSum(final Triplet triplet, final int sum) {
			this.triplet = triplet;
			this.sum = sum;
		}

		public TripletWithRunningSum foldSum() {
			return new TripletWithRunningSum(triplet.replaceIfBigger(sum), 0);
		}

		public TripletWithRunningSum addToSum(final int n) {
			return new TripletWithRunningSum(triplet, sum + n);
		}

		public static TripletWithRunningSum combine(final TripletWithRunningSum t1, final TripletWithRunningSum t2) {
			return new TripletWithRunningSum(Triplet.combine(t1.foldSum().triplet, t2.foldSum().triplet));
		}
	}
}
