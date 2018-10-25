import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
	private AtomicIntegerArray value;
	private byte maxval;
	
	GetNSetState(byte[] v) {
		value = new AtomicIntegerArray(v.length);
		for (int i = 0; i < v.length; i++) {
			value.set(i, (int) v[i]);
		}

		maxval = 127;
	}
	GetNSetState(byte[] v, byte m) {
		this(v);
		maxval = m;
	}

	public int size() {
		return value.length();
	}

	public byte[] current() {
		byte[] convertedArr = new byte[value.length()];

		for (int i = 0; i < value.length(); i++) {
			convertedArr[i] = (byte) value.get(i);
		}

		return convertedArr;
	}

	public boolean swap(int i, int j) {
		int vi = value.get(i);
		int vj = value.get(j);
		if (vi <= 0 || vj >= maxval) return false;

		value.set(i, vi - 1);
		value.set(j, vj + 1);

		return true;
	}
}


