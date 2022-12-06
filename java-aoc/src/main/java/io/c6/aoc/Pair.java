package io.c6.aoc;

import java.util.Objects;
import java.util.function.Function;

public final class Pair<L, R> {
	public final L left;
	public final R right;

	private Pair(final L left, final R right) {
		this.left = left;
		this.right = right;
	}

	public static <L, R> Pair<L, R> of(final L left, final R right) {
		return new Pair<>(left, right);
	}

	public <L1> Pair<L1, R> mapLeft(final Function<? super L, ? extends L1> mapper) {
		return Pair.of(mapper.apply(left), right);
	}

	public <R1> Pair<L, R1> mapRight(final Function<? super R, ? extends R1> mapper) {
		return Pair.of(left, mapper.apply(right));
	}

	public <L1, R1> Pair<L1, R1> map(
			final Function<? super L, ? extends L1> leftMapper,
			final Function<? super R, ? extends R1> rightMapper) {
		return Pair.of(leftMapper.apply(left), rightMapper.apply(right));
	}

	@Override
	public int hashCode() {
		return Objects.hash(left, right);
	}

	@Override
	public boolean equals(Object obj) {
		return this == obj ||
				(obj instanceof Pair &&
						Objects.equals(left, ((Pair<?, ?>) obj).left) &&
						Objects.equals(right, ((Pair<?, ?>) obj).right));
	}

	@Override
	public String toString() {
		return String.format("(%s, %s)", left, right);
	}
}
