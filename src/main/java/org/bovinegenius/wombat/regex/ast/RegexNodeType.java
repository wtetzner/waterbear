package org.bovinegenius.wombat.regex.ast;

public enum RegexNodeType {
  Character,
  Alternate,
  Sequence,
  KleeneStar,
  Plus,
  Question,
  CaptureGroup
}
