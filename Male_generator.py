from random import shuffle


females = open("1993_females_pop.csv")


Chroms = list()
Chroms_random = list()

females.readline()
for i in females:
    line= i.strip("\n")
    #print(line)
    Zsplit = list(line.split(","))
    #print(Zsplit)
    ChromoHap = Zsplit[1:16:2]
    #print(ChromoHap)
    if Zsplit[9] == str(1):
        Chroms.append(ChromoHap)
        Chroms_random.insert(0,ChromoHap)
        Chroms.append(ChromoHap)
        Chroms_random.insert(0,ChromoHap)
        #print(Zsplit)
    else:
        Chroms.insert(0,ChromoHap)
        Chroms_random.append(ChromoHap)
        Chroms.insert(0,ChromoHap)
        Chroms_random.append(ChromoHap)


#shuffle(Chroms_random)

#print(Chroms_random)

#print(len(Chroms_random), len(Chroms))

Male_Chroms = list()

i=0
count = 0
while i < 200:
    Temp_random_chrom = Chroms_random.pop(0)
    Temp_chrom = Chroms.pop()
    list = [None]*(len(Temp_chrom)+len(Temp_random_chrom))
    list[::2] = Temp_chrom
    list[1::2] = Temp_random_chrom
    if (Temp_chrom[4] == Temp_random_chrom[4]):
        Chroms_random.insert(0, Temp_random_chrom)
        Chroms.append(Temp_chrom)
        shuffle(Chroms_random)
        shuffle(Chroms)
        #print(list)
    else:
        #print(list)
        Male_Chroms.append(list)
        i+=1


    print(i)


#print(len(Male_Chroms2))


# while True:
#     shuffle(Chroms_random)
#     Male_Chroms = list()
#     Hom_Chroms = list()
#     for i in range(0,len(Chroms)):
#         temp_chrom = list()
#         for i2 in range(0,len(Chroms[i])):
#             temp_chrom.append(Chroms[i][i2])
#             temp_chrom.append(Chroms_random[i][i2])
#
#         if (temp_chrom[8] == temp_chrom[9] or temp_chrom[10] == temp_chrom[11]):
#             Hom_Chroms.append(temp_chrom)
#
#         else:
#             Male_Chroms.append(temp_chrom)
#     if len(Hom_Chroms) < 20:
#         break


for i in Male_Chroms:
    print(i)




