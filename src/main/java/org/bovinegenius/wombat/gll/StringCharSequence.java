package org.bovinegenius.wombat.gll;

import lombok.Value;

@Value(staticConstructor="of")
public class StringCharSequence implements CharSequence {
  private final String string;
  private final int start;
  private final int end;
  
  @Override
  public int length() {
    return end - start;
  }

  @Override
  public char charAt(int index) {
    return string.charAt(index + start);
  }

  @Override
  public CharSequence subSequence(int start, int end) {
    return new StringCharSequence(string, start + this.start, end + this.start);
  }
}
