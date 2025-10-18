# Test distribution simplifications
#
# For most verbs, distribution simplifications are helpful conceptually and
# for efficiency. For example, the logarithm of a Normal distribution is a
# Log Normal; a mixture of Finite distributions is also Finite. Parameter
# combinations for each verb that results in such simplifications are covered
# in the `special_distributions` internal object.
#
# To test that the correct simplified distribution was identified, one only
# needs to check that one of the representations of the simplified distribution
# matches that of the standard implementation. Not all representations need to
# be checked, because in all cases, a valid distribution object is returned,
# and the internal consistency of each distribution family has already been
# checked (either in distplyr for verb-based distributions, or in distionary
# for standard distribution families).
