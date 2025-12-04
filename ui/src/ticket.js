function addBranchToFileTicket(t, b) {
  if (b == null) {
    return t;
  }
  let parts = t.split(':');
  parts[0] = parts[0] + '@' + b;
  return parts.join(':');
}

export default {
  addBranchToFileTicket,
}

