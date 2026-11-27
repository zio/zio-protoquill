package zio.quill.parser.engine

import zio.quill.Spec

class PrefilterSpec extends Spec {
  "type prefilters reject unrelated expressions before attempting patterns" in {
    PrefilterProbe(false, true) mustEqual ((1, 0, 1, true))
  }
  "accepted expressions match once through orElse and lift" in {
    PrefilterProbe(true, true) mustEqual ((1, 1, 0, true))
  }
  "a matching type with no matching pattern reaches the fallback once" in {
    PrefilterProbe(true, false) mustEqual ((1, 1, 1, true))
  }
}
