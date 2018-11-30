import pandas
import matplotlib.pyplot as plt
import numpy as np

CNs = ['A implies C', 'A implies -C', '-A implies C', '-A implies -C',
       'C implies A', 'C implies -A', '-C implies A', '-C implies -A',
       'A ind. C']
BIAS = {1: "none", 2: "lawn-nn", 3: "pizza", 4: "douven1"}
PROBS2IDX = {0: "p_CA", 1: "p_CNA", 2: "p_NCA", 3: "p_NCNA"}


def getData(fn_joint, fn_marginal):
    joint = pandas.read_csv(fn_joint, index_col=0)
    marginal = pandas.read_csv(fn_marginal, index_col=0)

    return joint, marginal


def tablePlots(fn_j, fn_m, listener):
    joint, marginal = getData(fn_j, fn_m)

    for k, bias in BIAS.items():
#     k=1; bias="none"
        cols = [c for c in list(joint.columns) if c.endswith(str(k))]
        fig, axes = plt.subplots(nrows=3, ncols=3, figsize=(5.5, 5.5))
        barwidth = 0.1
        xpos = {'col1': (0.25-barwidth/2, 0.25+barwidth/2), 'col2': (0.75-barwidth/2, 0.75+barwidth/2)}
        ymin = {'row1': 1, 'row2': 0}
        for r, row_axes in enumerate(axes):
            for c, ax in enumerate(row_axes):
                cn = CNs[r*3 + c]

                data = joint.loc[cn][cols]

                ax.plot((0.5, 0.5), (2, 0), color='black')
                ax.plot((1, 0), (1, 1), color='black')
                for idx in range(4):
                    p = data[PROBS2IDX[idx] + "." + str(k)]
                    if idx == 0:
                        xs, y_min = xpos['col1'], ymin['row1']
                        p = p + 1
                    elif idx == 1:
                        xs, y_min = xpos['col2'], ymin['row1']
                        p = p + 1
                    elif idx == 2:
                        xs, y_min = xpos['col1'], ymin['row2']
                    else:
                        xs, y_min = xpos['col2'], ymin['row2']

                    # plot
                    ax.fill_between(xs, [y_min, y_min], [p, p], color='b')
                    ax.set_ylim([0, 2])
                    ax.set_xlim([0, 1.2])
                    ax.set_xticks([])
                    ax.set_yticks([0.1, 0.5, 0.9, 1.2, 1.6, 2.0])
                    ax.set_yticklabels([0.1, 0.5, 0.9, 0.1, 0.5, 0.9])
                    ax.set_frame_on(False)

                # marginal bar
                p_cn = marginal[cn][bias]
                ax.fill_between([1, 1.05], [0, 0], [p_cn, p_cn], color='r')

                p = " (" + str(np.round(p_cn, 2)) + ")"
                ax.set_title(cn + p, fontsize=8)

        fig.suptitle("Bias: " + bias)
        fig.subplots_adjust(hspace=0.4, wspace=0.2)
        plt.savefig(bias + '-' + listener + '.png')

if __name__ == '__main__':

    listener = "LL" # PL or LL

    fn_joint = './../rwebppl/results-' + listener + '-3-runs-each-bias/evs-joint.csv'
    fn_marginal = './../rwebppl/results-' + listener + '-3-runs-each-bias/evs-marginal.csv'

    tablePlots(fn_joint, fn_marginal, listener)


