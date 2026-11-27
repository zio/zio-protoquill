package zio.quill.ported.quotationspec

import zio.quill.parser.DoSerialize
import zio.quill.parser.SerializationBehavior

// Will disable AST serialization for quotes in packages/subpackages with this inside.
given DoSerialize with {
  override type BehaviorType = SerializationBehavior.SkipSerialize}
