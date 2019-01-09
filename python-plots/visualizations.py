import argparse
import pandas
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.font_manager import FontProperties

CNs = ['A implies C', 'A implies -C', '-A implies C', '-A implies -C',
       'C implies A', 'C implies -A', '-C implies A', '-C implies -A',
       'A ind. C']
BIAS = {1: "none", 2: "lawn", 3: "pizza", 4: "douven1"}
# BIAS = {1:"none"}
PROBS2IDX = {0: "p_CA", 1: "p_CNA", 2: "p_NCA", 3: "p_NCNA"}


def getData(fn_conditioned, fn_marginal):
    conditioned = pandas.read_csv(fn_conditioned, index_col=0)
    marginal = pandas.read_csv(fn_marginal, index_col=0)

    return conditioned, marginal


def tablePlots(fn_j, fn_m, listener):
    conditioned, marginal = getData(fn_j, fn_m)
    cs = list(conditioned.columns)
    print(cs)
    biases = [int(s[-1]) for s in cs]

    for k, bias in BIAS.items():
        if k not in biases:
            continue
#     k=1; bias="none"
        cols = [c for c in list(conditioned.columns) if c.endswith(str(k))]
        fig, axes = plt.subplots(nrows=3, ncols=3, figsize=(5.5, 5.5))
        barwidth = 0.1
        xpos = {'col1': (0.25-barwidth/2, 0.25+barwidth/2), 'col2': (0.75-barwidth/2, 0.75+barwidth/2)}
        ymin = {'row1': 1, 'row2': 0}
        for r, row_axes in enumerate(axes):
            for c, ax in enumerate(row_axes):
                cn = CNs[r*3 + c]

                data = conditioned.loc[cn][cols]

                ax.plot((0.5, 0.5), (2, 0), color='black')
                ax.plot((1, 0), (1, 1), color='black')
 		ax.text(0.135, 1.85, 'C,A', fontsize=6)
		ax.text(0.635, 1.85, 'C,-A', fontsize=6)
		ax.text(0.135, 0.8, '-C,A', fontsize=6)
		ax.text(0.635, 0.8, '-C,-A', fontsize=6)
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
                    
		    if c == 0:
		      ax.set_yticks([0, 0.5, 1, 1.5, 2.0])
                      ax.set_yticklabels([0, 0.5, '1/0', 0.5, 1.0])
		      ax.yaxis.set_ticks_position('left')
		    elif c==2:
		      ax.set_yticks([0, 0.5, 1, 1.5, 2.0])
                      ax.set_yticklabels([0, 0.5, '1/0', 0.5, 1.0])
		      ax.yaxis.set_ticks_position('right')
		    
		    else:
                      ax.set_yticks([])                  

		    ax.set_frame_on(False)
		    ax.tick_params(labelsize=10)
                # marginal bar
                p_cn = marginal[cn][bias]
                ax.fill_between([1, 1.05], [0, 0], [p_cn, p_cn], color='r')

                p = " (" + str(np.round(p_cn, 2)) + ")"
                ax.set_title(cn + p, fontsize=8)

        fig.suptitle("Bias: " + bias)
        fig.subplots_adjust(hspace=0.4, wspace=0.25)
        plt.savefig(bias + '-' + listener + '.png')

if __name__ == '__main__':

    parser = argparse.ArgumentParser(description='make 3x3 conditional probability probs for LL/PL')
    parser.add_argument('lt', type=str, help='LL/PL')
    parser.add_argument('--fn', type=str, default='', help='path to folder containing evs-conditioned.csv and evs-marginal.csv')
	
    args = parser.parse_args()
    if args.fn:
      fn_conditioned = args.fn + 'evs-conditioned.csv'
      fn_marginal = args.fn + 'evs-marginal.csv'
    else: 
      fn_conditioned = './../rwebppl/results-all-biases/' + args.lt + '/evs-conditioned.csv'
      fn_marginal = './../rwebppl/results-all-biases/' + args.lt + '/evs-marginal.csv'

    tablePlots(fn_conditioned, fn_marginal, args.lt)



