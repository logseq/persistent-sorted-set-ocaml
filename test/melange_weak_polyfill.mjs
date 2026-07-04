function canWeakRef(value) {
  return (
    (typeof value === "object" && value !== null) || typeof value === "function"
  );
}

function weakEntry(value) {
  if (canWeakRef(value)) {
    if (typeof WeakRef !== "function") {
      return undefined;
    }
    return { kind: "weak", ref: new WeakRef(value) };
  }
  return { kind: "strong", value };
}

function derefEntry(entry) {
  if (entry === undefined) {
    return undefined;
  }
  if (entry.kind === "weak") {
    return entry.ref.deref();
  }
  return entry.value;
}

globalThis.caml_weak_create = function camlWeakCreate(length) {
  return new Array(length + 2);
};

globalThis.caml_ephe_set_key = function camlEpheSetKey(ephe, offset, value) {
  ephe[offset + 2] = weakEntry(value);
  return 0;
};

globalThis.caml_ephe_unset_key = function camlEpheUnsetKey(ephe, offset) {
  ephe[offset + 2] = undefined;
  return 0;
};

globalThis.caml_weak_get = function camlWeakGet(ephe, offset) {
  return derefEntry(ephe[offset + 2]);
};

globalThis.caml_weak_get_copy = globalThis.caml_weak_get;

globalThis.caml_weak_check = function camlWeakCheck(ephe, offset) {
  return derefEntry(ephe[offset + 2]) !== undefined;
};

globalThis.caml_weak_blit = function camlWeakBlit(
  source,
  sourceOffset,
  target,
  targetOffset,
  length,
) {
  for (let index = 0; index < length; index += 1) {
    target[targetOffset + index + 2] = source[sourceOffset + index + 2];
  }
  return 0;
};
