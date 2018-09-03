import itertools
import numpy as np
import csv


def pCA(joint_table):
    return (joint_table[0])


def pCNA(joint_table):
    return joint_table[1]


def pNCA(joint_table):
    return joint_table[2]


def pNCNA(joint_table):
    return joint_table[3]


def pAgivenC(joint_table):
    return (joint_table[0]/(joint_table[0] + joint_table[1]))


def pA(joint_table):
    return (joint_table[0] + joint_table[2])


def pC(joint_table):
    return (joint_table[0] + joint_table[1])


def pCgivenA(joint_table):
    return (joint_table[0]/(joint_table[0] + joint_table[2]))


def write_tables_to_csv(bn, tables):
    with open(bn + '.csv', 'w', newline='') as csvfile:
        writer = csv.writer(csvfile, delimiter='\t')
        for table in tables:
            writer.writerow(table)


def compute_prob_tables(probs, bn):
    if bn == 'c->a':
        joint = [probs[0] * probs[2], (1-probs[0]) * probs[2],
                 probs[1] * (1-probs[2]), (1-probs[1]) * (1-probs[2])]
    elif bn == 'a->c':
        joint = [probs[0] * probs[2], probs[1] * (1-probs[2]),
                 (1-probs[0]) * probs[2], (1-probs[1]) * (1-probs[2])]
    elif bn == 'a||c':
        joint = [probs[0] * probs[1], (1-probs[0]) * probs[1],
                 (1-probs[1]) * probs[0], (1-probs[1]) * (1-probs[0])]
    else:
        raise ValueError("unknown bayes net: %s", bn)

    return joint


def get_all_joint_probabilities(granularity, write_csv=False):
    uniform_ps = midBins(granularity)

    combinations2 = list(itertools.product(uniform_ps, repeat=2))
    combinations3 = list(itertools.product(uniform_ps, repeat=3))

    all_joints = []

    print('#tables for each conditional BN: ' + str(len(combinations3)))
    tables_c_implies_a = []
    tables_a_implies_c = []

    nb_independent_ca = 0
    nb_independent_ac = 0
    for combi in combinations3:
        probs = list(combi)
        # Bayes Net C -> A
        # 0. sample: P(A|C)
        # 1. sample: P(A|not C)
        # 2. sample: P(C)
        joint = compute_prob_tables(probs, 'c->a')

        if joint not in all_joints:
            all_joints.append(joint)
#         tables_c_implies_a.append(joint + probs)
        tables_c_implies_a.append(joint)

        ca = pCgivenA(joint)
        c = pC(joint)
        ac = pAgivenC(joint)
        a = pA(joint)
        if ca == c or ac == a:
            nb_independent_ca += 1

        # Bayes Net A -> C
        # 0. sample: P(C|A)
        # 1. sample: P(C|not A)
        # 2. sample: P(A)
        joint = compute_prob_tables(probs, 'a->c')

        if joint not in all_joints:
            all_joints.append(joint)
#         tables_a_implies_c.append(joint + probs)
        tables_a_implies_c.append(joint)
        ca = pCgivenA(joint)
        c = pC(joint)
        ac = pAgivenC(joint)
        a = pA(joint)
        if ca == c or ac == a:
            nb_independent_ac += 1
    print('ratio independent tables from all BN c->a tables: ' + str(100*nb_independent_ca/len(combinations3)) + ' %')
    print('ratio independent tables from all BN a->c tables: ' + str(100*nb_independent_ac/len(combinations3)) + ' %')

    ratio_conditionals = len(all_joints)/(len(combinations3)*2)
    print('ratio different tables in conditonal prob tables: ' + str(ratio_conditionals))

    # Bayes Net A || C
    # 0. sample: P(A)
    # 1. sample: P(C)
    count = 0
    count_ca = 0
    count_ac = 0
    tables_a_ind_c = []
    print('#tables for BN A||C: ' + str(len(combinations2)))
    for combi in combinations2:
        probs = list(combi)
        joint = compute_prob_tables(list(combi), 'a||c')

        if joint not in all_joints:
            count += 1
            all_joints.append(joint)

        tables_a_ind_c.append(joint+probs)

        if joint in tables_a_implies_c:
            count_ac += 1
        if joint in tables_c_implies_a:
            count_ca += 1

    if write_csv:
        write_tables_to_csv('a-implies-c', tables_a_implies_c)
        write_tables_to_csv('c-implies-a', tables_c_implies_a)
        write_tables_to_csv('a-ind-c', tables_a_ind_c)

    print('#new tables for BN A ind. C (not in conditional tables): ' + str(count))
    print(str(count_ca/len(tables_a_ind_c)) + ' of tables in BN A||C are in tables of BN c->a.')
    print(str(count_ac/len(tables_a_ind_c)) + ' of tables in BN A||C are in tables of BN a->c.')

    return np.array(all_joints)


def midBins(granularity):
    midBins = [roundTo3(x/granularity + 1/(2*granularity)) for x in range(granularity)]
    return midBins


def roundTo3(x):
    return np.round(x*1000)/1000


if __name__ == '__main__':
    vals = get_all_joint_probabilities(5)
