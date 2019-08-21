import simuPOP as sim
from math import log

Replicates = 1000
Fixed_pop_size = 450
Fixed_pop_size2 = 200
Homozygote_fitness = 1
Offspring_Poisson_dist_mean = 5
Output_file = ">>simuPop_1993start_2pop_Recal_R"+str(Replicates)+"_FP"+str(Fixed_pop_size)+"_HF"+str(Homozygote_fitness)+"_OPDmean"+str(Offspring_Poisson_dist_mean)+".txt"
#Output_file2 = ">>simuPop_1993start_2pop_ShannonE_R"+str(Replicates)+"_FP"+str(Fixed_pop_size)+"_HF"+str(Homozygote_fitness)+"_OPDmean"+str(Offspring_Poisson_dist_mean)+".txt"
Output_file3 = ">>simuPop_1993start_2pop_AlleleNum_R"+str(Replicates)+"_FP"+str(Fixed_pop_size)+"_HF"+str(Homozygote_fitness)+"_OPDmean"+str(Offspring_Poisson_dist_mean)+".txt"


def importData(filename):
    'Read data from ``filename`` and create a population'
    data = open(filename)
    header = data.readline()
    fields = header.split(',')
    # columns 1, 3, 5, ..., without trailing '_1'
    names = [fields[x].strip()[:-2] for x in range(1, len(fields), 2)]
    popSize = 0
    alleleNames = set()
    for line in data.readlines():
        # get all allele names
        alleleNames |= set([x.strip() for x in line.split(',')[1:]])
        popSize += 1
    # create a population
    alleleNames = list(alleleNames)
    pop = sim.Population(size=popSize, loci=len(names), lociNames=names,
        alleleNames=alleleNames,chromTypes=sim.CHROMOSOME_X,infoFields='fitness')
    # start from beginning of the file again
    data.seek(0)
    # discard the first line
    data.readline()
    for ind, line in zip(pop.individuals(), data.readlines()):
        fields = [x.strip() for x in line.split(',')]
        if fields[0] == '1':
            sex = sim.MALE
            ploidy0 = [alleleNames.index(fields[x]) for x in range(1, len(fields), 2)]
            ind.setGenotype(ploidy0, 0)
            ind.setSex(sex)
        else:
            sex = sim.FEMALE
            ploidy0 = [alleleNames.index(fields[x]) for x in range(1, len(fields), 2)]
            ploidy1 = [alleleNames.index(fields[x]) for x in range(2, len(fields), 2)]
            ind.setGenotype(ploidy0, 0)
            ind.setGenotype(ploidy1, 1)
            ind.setSex(sex)
    # close the file
    data.close()
    return pop



import simuPOP as sim
import simuPOP.utils as utils


pop1 = importData('1993_femalesx2+200fakemales.txt')



def sel(geno):
    #     BB  Bb   bb
    # AA  1   1    1
    # Aa  1   1-s1 1-s2
    # aa  1   1    1-s2
    #
    # geno is (A1 A2 B1 B2)
    if len(geno) == 1:
        v = 1
        #print(geno[0],v)
        return v
    elif geno[0] == geno[1]:
        v = Homozygote_fitness  # case of AaBb
        #print(geno[0], geno[1],v)
        return v
    else:
        v = 1
        #print(geno[0], geno[1], v)
        return v

def sizeChange(gen):
    if gen <= 40:
        v = Fixed_pop_size
        #print(geno[0],v)
        return v
    else:
        v = Fixed_pop_size2
        #print(geno[0], geno[1], v)
        return v



simu = sim.Simulator(pop1,rep=Replicates)


simu.evolve(
    initOps=[
    ],
    preOps=[

                        ],
    matingScheme=sim.RandomMating(subPopSize=sizeChange,numOffspring=(sim.POISSON_DISTRIBUTION, Offspring_Poisson_dist_mean),ops=sim.Recombinator(loci=[0, 1, 2, 3, 4, 5, 6, 7], rates=[0.095162582,	0.075960076,	0.094257292,	0.154646165,	0.005,	0.104165865,	0.071328306,0.5]))

    ,
    postOps=[
            sim.PySelector(loci=[4], func=sel),
            sim.Stat(alleleFreq=[0,1,2,3,4,5,6,7],numOfMales=True,popSize=True),

            #Output H per replicate per generation to file
            sim.PyEval(r"'%d\t%d\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\n' % (rep,gen, 1-sum([x*x for x in alleleFreq[0].values()]), 1-sum([x*x for x in alleleFreq[1].values()]), 1-sum([x*x for x in alleleFreq[2].values()]), 1-sum([x*x for x in alleleFreq[3].values()]), 1-sum([x*x for x in alleleFreq[4].values()]), 1-sum([x*x for x in alleleFreq[5].values()]), 1-sum([x*x for x in alleleFreq[6].values()]), 1-sum([x*x for x in alleleFreq[7].values()]))", step=1,reps=(range(0,Replicates,1)),output=Output_file),

            sim.PyExec('from math import log'),

            #sim.Dumper(output='!"./Simulation_output_pops/pop%d.pop"%rep',step=69,max=500,width=3),

            #allelic richness for given locus
            #sim.PyEval(r"'%d\n' % (len(alleleNum[0].keys()))"),

            #Output Shannon E per replicate per generation to file
            #sim.PyEval(r"'%d\t%d\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\n' % (rep,gen, (0-sum([(x * log(x)) for x in alleleFreq[0].values()])/log(len(alleleNum[0].keys())) if len(alleleNum[0].keys()) > 1 else 0) , (0-sum([(x * log(x)) for x in alleleFreq[1].values()])/log(len(alleleNum[1].keys())) if len(alleleNum[1].keys()) > 1 else 0) , (0-sum([(x * log(x)) for x in alleleFreq[2].values()])/log(len(alleleNum[2].keys())) if len(alleleNum[2].keys()) > 1 else 0) , (0-sum([(x * log(x)) for x in alleleFreq[3].values()])/log(len(alleleNum[3].keys())) if len(alleleNum[3].keys()) > 1 else 0) , (0-sum([(x * log(x)) for x in alleleFreq[4].values()])/log(len(alleleNum[4].keys())) if len(alleleNum[4].keys()) > 1 else 0) , (0-sum([(x * log(x)) for x in alleleFreq[5].values()])/log(len(alleleNum[5].keys())) if len(alleleNum[5].keys()) > 1 else 0) , (0-sum([(x * log(x)) for x in alleleFreq[6].values()])/log(len(alleleNum[6].keys())) if len(alleleNum[6].keys()) > 1 else 0), (0-sum([(x * log(x)) for x in alleleFreq[7].values()])/log(len(alleleNum[7].keys())) if len(alleleNum[7].keys()) > 1 else 0))", step=1,reps=(range(0,Replicates,1)),output=Output_file2)

            #Output AlleleNum
            sim.PyEval(r"'%d\t%d\t%.d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n' % (rep,gen, len(alleleNum[0].keys()),len(alleleNum[1].keys()),len(alleleNum[2].keys()),len(alleleNum[3].keys()),len(alleleNum[4].keys()),len(alleleNum[5].keys()),len(alleleNum[6].keys()),len(alleleNum[7].keys()))", step=1,reps=(range(0,Replicates,1)),output=Output_file3)

            #Output sex ratio
            #sim.PyExec('ratio= (numOfMales/popSize)'),
            #sim.PyEval("'gen=%d' % gen", reps=0),
            #sim.PyEval(r"'\t%.3f' % (ratio)"),
            #sim.PyOutput('\n', reps=-1)

    ],
    gen = 70)

