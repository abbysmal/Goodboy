def something(opcode, mnemo, ocount, o1, o2, osize, cycles):
    o1 = 'Some "{}"'.format(o1) if o1 else "None"
    o2 = 'Some "{}"'.format(o2) if o2 else "None"
    cy = "/".join(str(x) for x in cycles)
    s = '{{ mnemo = "{}"; operand_count = {}; operand1 = {}; operand2 = {}; size = {}; cycles = "{}"; }}'.format(mnemo, ocount, o1, o2, osize, cy)
    return s


for k, v in cbprefixed.items():
    m = v["mnemonic"]
    oc = v["operand_count"]
    o1 = v["operand1"] if (oc >= 1)  else None
    o2 = v["operand2"] if (oc >= 2)  else None
    o = something(k, m, oc, o1, o2, v["bytes"], v["cycles"])
    print('| {} -> {}'.format(k, o))
