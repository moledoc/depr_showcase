
import airflow
import datetime
import urllib.request as request
import pandas as pd
from airflow import DAG
from airflow.operators.bash_operator import BashOperator
from airflow.operators.python_operator import PythonOperator, BranchPythonOperator
from airflow.operators.dummy_operator import DummyOperator
from airflow.operators.postgres_operator import PostgresOperator
import numpy as np
import json
import random
import requests

nrOfCharacter=5

default_args_dict = {
    'start_date': datetime.datetime(2020, 11, 17, 0, 0, 0),
    'concurrency': 4,
    'schedule_interval': "0/"+str(int(60/nrOfCharacter))+" 0 * * 5", # friday at 00:00
    'retries': 1,
    'retry_delay': datetime.timedelta(minutes=0.5),
}

hw_dag = DAG(
    dag_id = 'hw_dag',
    default_args=default_args_dict,
    catchup=False,
    # is_paused_upon_creation=False,
    template_searchpath=['/usr/local/airflow/data/']
)

b1_attr = BashOperator(
    task_id = 'gen_dndattr',
    dag=hw_dag,
    bash_command = "echo \"[$(shuf -i 2-18 -n 6)]\" | tr \"\\n\" \"\,\" |\
            sed \"s/\],/\]\\n/g\" > /usr/local/airflow/data/dndattr",
    trigger_rule='all_success',
    # depends_on_past = False
)

b1_level = BashOperator(
    task_id = 'gen_dndlevel',
    dag=hw_dag,
    bash_command = "shuf -i 1-3 -n 1 > /usr/local/airflow/data/dndlevel",
    trigger_rule='all_success',
    # depends_on_past = False
)

b1_race = BashOperator(
    task_id = 'get_dndrace',
    dag=hw_dag,
    bash_command = "curl -G https://www.dnd5eapi.co/api/races |\
            python3 -c \"import sys,json,random;\
            list=json.load(sys.stdin)['results'];\
            results_list = [{results['index']: results['url']} for results in list];\
            print(random.choice(results_list))\" \
            > /usr/local/airflow/data/dndrace",
    trigger_rule='all_success',
    # depends_on_past = False
)

b1_class = BashOperator(
    task_id = 'get_dndclass',
    dag=hw_dag,
    bash_command = "curl -G https://www.dnd5eapi.co/api/classes |\
            python3 -c \"import sys,json,random;\
            list=json.load(sys.stdin)['results'];\
            results_list = [{results['index']: results['url']} for results in list];\
            print(random.choice(results_list))\" \
            > /usr/local/airflow/data/dndclass",
    trigger_rule='all_success',
    # depends_on_past = False
)

b1_has_spells0 = BashOperator(
    task_id = 'has_spells0',
    dag=hw_dag,
    bash_command = "class=$(cat /usr/local/airflow/data/dndclass | sed \"s/{\\|}\\|'//g\");\
            classIdx=$(echo ${class%%:*});\
            class=$(echo ${class##*\ });\
            curl -G \"https://www.dnd5eapi.co$class/spells\" |\
            python3 -c \"import sys,json,random;\
            list=json.load(sys.stdin)['results'];\
            results_list = [results['index'] for results in list];\
            print(results_list)\"\
            > /usr/local/airflow/data/dndspells;\
            echo $classIdx > /usr/local/airflow/data/dndclassIdx",
    # var $class is in form /api/classes/<classname>
    trigger_rule='all_success',
    # depends_on_past = False
)

def _has_spells():
    f = open("/usr/local/airflow/data/dndspells")
    spells = f.readline().strip()
    f.close()
    if spells != '[]':
        return "get_dndspells"
    else:
        return "no_spells"
    
b1_has_spells = BranchPythonOperator( 
    task_id = 'has_spells',
    dag=hw_dag,
    python_callable=_has_spells,
    trigger_rule='all_success',
    # depends_on_past = False
)

def _get_spells():
    f=open('/usr/local/airflow/data/dndspells')
    line=f.readline().strip("[|]|\n").split("', '")
    f.close()
    f=open('/usr/local/airflow/data/dndlevel')
    level=int(f.readline().strip())
    f.close()
    spellList=[]
    howManySpells = level+min(3,len(line))
    i = 0
    while True:
        if i == howManySpells:
            break
        spellVal = random.choice(line).strip("'")
        r = requests.get('https://www.dnd5eapi.co/api/spells/'+spellVal).content.decode("UTF-8")
        spellLevel = int(json.loads(r)['level'])
        if spellLevel <= 2: #and spellVal not in spellList:
            spellList.append(spellVal)
            i+=1
    f=open('/usr/local/airflow/data/dndspells',mode="w")
    f.write(str(spellList))
    f.close()
            
b1_spells = PythonOperator( 
    task_id = 'get_dndspells',
    dag=hw_dag,
    python_callable=_get_spells,
    trigger_rule='all_success',
    # depends_on_past = False
)


join_tasks_a = DummyOperator(
    task_id='join_tasks_a',
    dag=hw_dag,
    trigger_rule='none_failed'
)

no_spells = DummyOperator(
    task_id='no_spells',
    dag=hw_dag,
    trigger_rule='none_failed'
)

# # This function could be nicer.
# def _gen_npl():
#     f=open('/usr/local/airflow/data/dndclassIdx')
#     classIdx=f.readline().strip()
#     f.close()

#     f=open('/usr/local/airflow/data/dndrace')
#     race=f.readline().strip("{|}|\n|'").split("': ")[0]
#     f.close()
#     f=open('/usr/local/airflow/data/dndraceIdx','w')
#     f.write(race)
#     f.close()
#     # name
#     nr = random.choice(range(1,10001))
#     f=open('/usr/local/airflow/data/dndname','w')
#     f.write(classIdx+"."+race+"."+str(nr))
#     f.close()

#     # language
#     r = requests.get('https://www.dnd5eapi.co/api/races/'+race).content.decode("UTF-8")
#     listLang = json.loads(r)['languages']
#     results_list = [results['index'] for results in listLang]
#     nr_of_lang=random.randint(1,len(results_list))
#     lang=random.sample(results_list,nr_of_lang)
#     f=open('/usr/local/airflow/data/dndlang','w')
#     f.write(str(lang))
#     f.close()

#     # proficiency_choices
#     r = requests.get('https://www.dnd5eapi.co/api/classes/'+classIdx).content.decode("UTF-8")
#     listProf = json.loads(r)['proficiency_choices']
#     results_list = [results['index'] for results in listProf[0]['from']]
#     nr_of_prof=random.randint(1,len(results_list))
#     prof=random.sample(results_list,nr_of_prof)
#     f=open('/usr/local/airflow/data/dndprof','w')
#     f.write(str(prof))
#     f.close()

# b2_gen_npl = PythonOperator(
#     task_id = 'gen_dnd_npl',
#     dag=hw_dag,
#     python_callable=_gen_npl,
#     trigger_rule='all_success',
#     # depends_on_past = False
# )


b2_gen_name = BashOperator(
    task_id = 'gen_dndname',
    dag=hw_dag,
    bash_command = "\
            class=$(cat /usr/local/airflow/data/dndclassIdx);\
            race=$(cat /usr/local/airflow/data/dndrace | sed \"s/{\|}\|'//g\");\
            race=$(echo ${race%%:*});\
            echo $race > /usr/local/airflow/data/dndraceIdx;\
            echo \"$class.$race.$(shuf -n 1 -i 1-10000)\" > /usr/local/airflow/data/dndname",
    trigger_rule='all_success',
    depends_on_past = True
)

b2_get_lang = BashOperator(
    task_id = 'get_dndlang',
    dag=hw_dag,
    bash_command = "race=$(cat /usr/local/airflow/data/dndrace | sed \"s/{\|}\|'//g\");\
            race=$(echo ${race%%:*});\
            curl -G \"https://www.dnd5eapi.co/api/races/$race\" |\
            python3 -c \"import sys,json,random;\
            list=json.load(sys.stdin)['languages'];\
            results_list = [results['index'] for results in list];\
            nr_of_lang=random.randint(1,len(results_list));\
            print(random.sample(results_list,nr_of_lang))\" \
            > /usr/local/airflow/data/dndlang",
    trigger_rule='all_success',
    # depends_on_past = True
)

b2_get_prof = BashOperator(
    task_id = 'get_dndprof',
    dag=hw_dag,
    bash_command = "\
            class=$(cat /usr/local/airflow/data/dndclassIdx);\
            curl -G \"https://www.dnd5eapi.co/api/classes/$class\" |\
            python3 -c \"import sys,json,random;\
            list=json.load(sys.stdin)['proficiency_choices'];\
            results_list = [results['index'] for results in list[0]['from']];\
            nr_of_prof=random.randint(1,len(results_list));\
            print(random.sample(results_list,nr_of_prof))\" \
            > /usr/local/airflow/data/dndprof",
    trigger_rule='all_success',
    # depends_on_past = True
)

join_tasks_b = DummyOperator(
    task_id='join_tasks_b',
    dag=hw_dag,
    trigger_rule='none_failed'
)

task_compose_pd_df = BashOperator(
    task_id = 'compose_pd_df',
    dag=hw_dag,
    bash_command = "\
            name=$(cat /usr/local/airflow/data/dndname);\
            attr=$(cat /usr/local/airflow/data/dndattr);\
            race=$(cat /usr/local/airflow/data/dndraceIdx);\
            languages=$(cat /usr/local/airflow/data/dndlang);\
            class=$(cat /usr/local/airflow/data/dndclassIdx);\
            proficiency_choices=$(cat /usr/local/airflow/data/dndprof);\
            level=$(cat /usr/local/airflow/data/dndlevel);\
            spells=$(cat /usr/local/airflow/data/dndspells);\
            echo \"name;attributes;race;languages;class;proficiency_choices;level;spells\" > /usr/local/airflow/data/character.csv;\
            echo \"$name;$attr;$race;$languages;$class;$proficiency_choices;$level;$spells\" >> /usr/local/airflow/data/character.csv",
    trigger_rule='all_success',
    # depends_on_past = True
)

def _create_character_query():
    df = pd.read_csv('/usr/local/airflow/data/character.csv',sep=';')
    with open("/usr/local/airflow/data/character_inserts.sql", "w") as f:
        df_iterable = df.iterrows()
        f.write(
            "CREATE TABLE IF NOT EXISTS characters (\n"
            "name VARCHAR(255),\n"
            "attributes VARCHAR(255),\n"
            "race VARCHAR(255),\n"
            "languages VARCHAR(255),\n"
            "class VARCHAR(255),\n"
            "proficiency_choices VARCHAR(255),\n"
            "level VARCHAR(255),\n"
            "spells VARCHAR(255)\n"
            ");\n"
        )
        for index, row in df_iterable:
            name = row['name']
            attributes = row['attributes']
            race = row['race']
            languages = row['languages']
            chrClass = row['class']
            proficiency_choices = row['proficiency_choices']
            level = row['level']
            spells= row['spells']

            f.write(
                "INSERT INTO characters VALUES ("
                f"'{name}', '{attributes}', '{race}', '{languages}', '{chrClass}', '{proficiency_choices}','{level}','{spells}'"
                ");\n"
            )
            f.close()

task_postgres_prep = PythonOperator(
    task_id='postgres_prep',
    dag=hw_dag,
    python_callable=_create_character_query,
    trigger_rule='all_success'
)

task_postgres = PostgresOperator(
    task_id='postgres',
    dag=hw_dag,
    postgres_conn_id='postgres_not_default',
    sql='character_inserts.sql',
    trigger_rule='all_success',
    autocommit=True
)

# job_cleanup = BashOperator(
#     task_id = 'cleanup',
#     dag=hw_dag,
#     bash_command = "rm /usr/local/airflow/data/*",
#     trigger_rule='all_success',
#     depends_on_past = True
# )

job_end = DummyOperator(
    task_id='end',
    dag=hw_dag,
    trigger_rule='none_failed'
)

b1_race >> join_tasks_a
b1_level >> join_tasks_a
b1_attr >> join_tasks_a
b1_class >> b1_has_spells0 >> b1_has_spells 
b1_has_spells >> [b1_spells,no_spells]
[b1_spells,no_spells] >> join_tasks_a
join_tasks_a >> b2_gen_name >> join_tasks_b
join_tasks_a >> b2_get_lang >> join_tasks_b
join_tasks_a >> b2_get_prof >> join_tasks_b
join_tasks_b >> task_compose_pd_df >> task_postgres_prep >> task_postgres >> job_end
