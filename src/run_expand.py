# -*- coding:utf-8 -*-

# on dit à la fonction main (qui est ce qui est appelé avec f5 ou f6 d'être utilisée avec les paramètres que l'on donne à sys.argv

import sys
import main #je laisse le main parce que je ne veux pas travailler sur le import, je laisse celui de LIAM2
from simulation import Simulation

chemin = 'C:\\Users\\a.eidelman\\Desktop\\GenIPP_Pyth\\liam\\'
chemin = 'M:\\Myliam2\\'
chemin = 'T:\\Myliam2\\'

print chemin
#import
sys.argv.append('import')
#sys.argv.append('C:\\Users\\a.eidelman\\Desktop\\GenIPP_Pyth\\liam\\test\\examples\\demo_import.yml')
fichier = chemin+'Genebios\\import_pat_Didier.yml'
fichier= chemin+'test\\small\\import.yml'
sys.argv.append(fichier)
# main.main()

## ici tourne Simulation.from_yaml et éventuellement Simulation.interactive
fichier= chemin+'Patrimoine\\duplication\\expand.yml'


# fichier= chemin+'test\\small\\simulation.yml'

#simulation= Simulation.from_yaml(fichier,
#                     input_dir=None,
#                    input_file=None,

#                    output_dir=None,
#                    output_file=None)
## return Simulation(globals_fields, periods, start_period,init_processes, init_entities, processes, entities, data_source, default_entity)
##TODO: on pourrait donc définir directement ces champs ! #TODO
#fichier='C:\\Users\\a.eidelman\\Desktop\\GenIPP_Pyth\\liam\\test\\examples\\demo04.yml'
#fichier='C:\\Users\\a.eidelman\\Desktop\\GenIPP_Pyth\\liam\\test\\small\\simulation.yml'
#fichier2='C:\\Users\\a.eidelman\\Desktop\\GenIPP_Pyth\\liam\\test\\examples\\demo02.yml'
simulation= Simulation.from_yaml(
                                 fichier,
                     input_dir=None,
                    input_file=None,
                    output_dir=None,                    
                    output_file=None)
#simulation= Simulation.from_multi_yaml([fichier,fichier],
#                     input_dir=None,
#                    input_file=None,
#                    output_dir=None,
#                    output_file=None)


#print simulation.globals_fields 
#print simulation.periods 
#print simulation.start_period 
#print simulation.init_processes 
#print simulation.init_entities 
#print simulation.processes 
#print simulation.entities
#print simulation.data_source 
#print simulation.default_entity 

#print simulation.stepbystep 
        
#simulation.run(False)

#simulation= Simulation.from_yaml(fichier2,
#                     input_dir=None,
#                    input_file=None,
#                    output_dir=None,
#                    output_file=None)
#print 'deuxieme'
simulation.run(False)
