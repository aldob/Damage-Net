# pyinstaller Damage_Net_v1.0.py --add-data "images;images" --add-data "database;database" --icon="images\damage_net_icon_v11.ico" --hidden-import="pkg_resources.py2_warn"
# Inno Setup Compiler damage_net_v1.0_compiler.iss

vers="1.0"
iconfile="images/damage_net_icon_v11.ico"

# fast_load loads most modules needed for the program to run then makes them global, but it itself is run AFTER mainloop
# this reduces perceived load time by importing most modules after the program has visually loaded
def fast_load():
    import os
    import sys
    import re
    import string
    from collections import OrderedDict
    from collections import OrderedDict
    from TkTreectrl.MultiListbox import MultiListbox
    from venn import venn
    from lifelines import KaplanMeierFitter, CoxPHFitter
    from seaborn import heatmap as snsheatmap
    from matplotlib.pyplot import figure, get_fignums, rcParams, close, gcf, subplots, subplots_adjust
    from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
    from pandas import ExcelFile, ExcelWriter, DataFrame
    from numpy import log2, average
    global OrderedDict
    global MultiListbox
    global venn
    global CoxPHFitter
    global KaplanMeierFitter
    global snsheatmap
    global figure
    global get_fignums
    global rcParams
    global close
    global gcf
    global subplots
    global subplots_adjust
    global FigureCanvasTkAgg
    global ExcelFile
    global ExcelWriter
    global DataFrame
    global log2
    global average

# import modules needed for loading screen
import tkinter as tk
from tkinter import *
import tkinter.ttk as ttk
from PIL import ImageTk
from PIL import Image

# Create the root window and start formatting it with a title and icon etc.
root=tk.Tk()
root.configure(background='white')
root.title("Damage Net v"+vers)
root.wm_iconbitmap(iconfile)
root.iconbitmap(default=iconfile)
root.geometry("+700+500")
root.protocol("WM_DELETE_WINDOW", sys.exit)

# loading label and icon image
load_text=tk.StringVar()
load_text.set("Loading kernel...")
load_label = tk.Label(root, text=load_text, font=("Calibri",12), bg='white', height=1)
load_label.pack(side=TOP, expand=1)
logo_canv = tk.Canvas(root, width=320, height=160, bg='white', highlightthickness=0)
logo_canv.pack(side=LEFT, anchor='w')
image = Image.open(iconfile)
tkimage = ImageTk.PhotoImage(image.resize((150,150),Image.ANTIALIAS))
canvas_obj = logo_canv.create_image(150, 75, image=tkimage)
root.update()

# custom border style setup
focusBorderImageData = '''
    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
    Ry/99NIz//oGrZpUUEAAOw==
'''
borderImageData = '''
    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
'''
style = ttk.Style()
borderImage = tk.PhotoImage("borderImage", data=borderImageData)
focusBorderImage = tk.PhotoImage("focusBorderImage", data=focusBorderImageData)
style.element_create("RoundedFrame", "image", borderImage, ("focus", focusBorderImage), border=16, sticky="nsew")
style.layout("RoundedFrame", [("RoundedFrame", {"sticky": "nsew"})])

# final imports and database connection
load_label.configure(text="Loading database links...")
root.update_idletasks()
from itertools import *
from sqlite3 import connect, Row
conn=connect("database/Damage_Netv1.0.db")
with open("database/settings.ini") as settings:
    for line in settings:
        if not line.startswith("["):
            exec(line)




# all the functions for the different program aspects
load_text.set("Loading framework...")
root.update_idletasks()
# Basic interaction functions
def importation(the_file):
    if the_file.endswith(".csv") or the_file.endswith(".tsv") or the_file.endswith(".txt"):
        contents=set()
        with open(the_file) as data_file:
            for data in data_file:
                for entry in re.split(',|;|:|\t', data):
                    contents.add(entry)
        filtered = {re.sub('\n|,|\t|;|:|"', "", x).strip().upper() for x in contents if re.sub('\n|,|\t|;|:', "", x).strip()}
    else:
        xl=ExcelFile(the_file)
        data=(xl.parse(xl.sheet_names[0], header=None, names=['a']))['a'].tolist()
        contents=set()
        for data2 in data:
            for entry in re.split(',|;|:|\t', data2):
                contents.add(entry)
        filtered = {re.sub('\n|,|\t|;|:', "", x).strip() for x in contents if re.sub('\n|,|\t|;|:', "", x).strip()}
    return(list(filtered))
def exporterer(output_tow):
    output_df = DataFrame.from_records(output_tow[1:], columns=output_tow[0])
    file_name = tk.filedialog.asksaveasfilename(defaultextension=".xlsx", filetypes=(("Excel Workbook", "*.xlsx"), ("Excel 97-2003 Workbook", "*.xls"), ("Comma delimited", ".csv"), ("Tab delimited", ".tsv"), ("Text (Tab delimited)", ".txt"), ("All Files", "*.*")))
    if not file_name.endswith((".xlsx", "xls")):
        with open(file_name, "w") as data:
            for row in output_tow:
                if file_name.endswith(".csv"):
                    data.write(",".join([str(r) for r in row])+"\n")
                else:
                    data.write("\t".join([str(r) for r in row])+"\n")
    else:
        with ExcelWriter(file_name) as writer:
            output_df.to_excel(writer, index=False)
def update_settings(setting, old, new):
    for key,value in eval(setting).items():
        if value==old:
            eval(setting)[key]=new
    new_settings=list()
    settings_file=open("database/settings.ini", "r+")
    settings=settings_file.readlines()
    for old_setting in settings:
        if old_setting.startswith(setting):
            new_settings.append(str(setting+"="+str(eval(setting))+"\n"))
        else:
            new_settings.append(old_setting)
    settings_file.seek(0)
    settings_file.truncate()
    for setting in new_settings:
        settings_file.write(setting)
    settings_file.close()



# Database interaction functions
def insert_data(dat, conne=conn, tabl="dnet_1"):
    data_insert = """ INSERT INTO {0}(code_name, written_name, category, title, DOI, method, results)
              VALUES(?,?,?,?,?,?,?) """
    cur = conne.cursor()
    cur.execute(data_insert.format(tabl), dat)
    conne.commit()
def data_query(query, conne=conn):
    cur = conne.cursor()
    if query.startswith("SELECT"):
        rows = cur.execute(query).fetchall()
        return(rows)
    else:
        cur.execute(query)
    conne.commit()
def retrieve_results(codename):
    total_results=str(data_query("SELECT results FROM dnet_1 WHERE code_name='"+codename+"'")[0])
    result_list = [(re.sub(r"\'|\)|\(|,", "", prot)).upper() for prot in total_results.split(";")]
    return(result_list)
def retrieve_col(tabl="dnet_1", retrieve="code_name", filter=None, select=None):
    if filter != None:
        raw_data=data_query("SELECT "+retrieve+" FROM "+tabl+" WHERE "+filter+"='"+select+"'")
    else:
        raw_data=data_query("SELECT "+retrieve+" FROM "+tabl)
    if len(raw_data)==1:
        proc_data=[re.sub(r"\'|\)|\(|,", "", str(raw_data[0]))]
    else:
        proc_data=[re.sub(r"\'|\)|\(|,", "", str(study)) for study in raw_data]
    return(proc_data)
def rename_category(tabl="dnet_1", old="", new=""):
    data_query("UPDATE "+tabl+" SET category = '"+new+"' WHERE category='"+old+"'")



# Data analysis functions
def powerset(iterable):
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(len(s)+1))
def overlap_venn(omics):
    over_dict=dict()
    close()
    rcParams.update({'font.weight':'normal'})
    for result in omics:
        res_data=set(retrieve_results(retrieve_col(retrieve="code_name", filter="written_name", select=result)[0]))
        over_dict.update({str(result.split(" - ")[0]+" - "+result.split(" - ")[-1]):res_data})
    fig1=venn(over_dict, legend_loc="upper right", fontsize=10)
    return(figure(get_fignums()[-1]))
def det_overlap(liststr):
    lists=[omicl for omicl in liststr.split("\n") if omicl!=""]
    if len(lists)<=1:
        return
    compares=[combo for combo in powerset(lists)]
    overlap_results=[["Comparison", "Overlap", "Genes"]]
    for comparison in compares:
        if len(comparison)>0:
            intersected=set(retrieve_results(retrieve_col(retrieve="code_name", filter="written_name", select=comparison[0])[0]))
            if len(comparison)>1:
                for results in comparison[1:]:
                    intersected=intersected.intersection(set(retrieve_results(retrieve_col(retrieve="code_name", filter="written_name", select=results)[0])))
            overlap_results.append([str(" + ".join(comparison)), str(len(intersected)), str(";".join(intersected))])
    if overlap_results:
        # Make the window
        result_window=tk.Tk()
        result_window.configure(background='white')
        result_window.title("Damage Net v"+vers)
        result_window.wm_iconbitmap(iconfile)
        result_window.geometry("1000x500+"+str(root.winfo_x()+200)+"+"+str(root.winfo_y()+31))
        topsep = ttk.Separator(result_window, orient=HORIZONTAL)
        topsep.pack(side=TOP, fill=X)
        # Make and pack the frames
        button_frame=tk.Frame(master=result_window, bg='white')
        button_frame.pack(side=TOP, anchor=NW)
        table_frame = tk.Frame(master=result_window, bg='white')
        table_frame.pack(side=TOP, fill=BOTH, expand=1)
        venn_frame=tk.Frame(master=result_window, bg='white')
        venn_frame.pack(side=BOTTOM, anchor=W, expand=1)   
        # Create the table and add it to the table frame    
        yscroll=Scrollbar(table_frame, orient="vertical")
        yscroll.pack(side=RIGHT, fill=Y)
        xscroll=Scrollbar(table_frame, orient="horizontal")
        result_mlb=MultiListbox(table_frame)
        result_mlb.pack(side=TOP, anchor=NW, fill=X)
        xscroll.pack(side=TOP, fill=X)
        result_mlb.focus_set()
        result_mlb.configure(columns=overlap_results[0], font=("Calibri",10), yscrollcommand=yscroll.set, xscrollcommand=xscroll.set, xscrollincrement=1, selectbackground='#a9f5e8', selectforeground='black', headerfont=("Calibri bold",12), width=100)
        for over_result in overlap_results[1:]:
            result_mlb.insert(tk.END, *over_result)       
        yscroll.config(command=result_mlb.yview)
        xscroll.config(command=result_mlb.xview)
        # Create the venn and add it to the venn frame
        report_text = tk.Label(venn_frame, font=("Calibri", 12), background='white', relief='flat')
        report_text.pack(side=TOP, anchor=NW)
        if len(lists) <=6:
            report_text.configure(text="Plot is a representation, export for full image.")
            over_venn=overlap_venn(lists)
            over_fig = FigureCanvasTkAgg(over_venn, venn_frame)
            over_fig.get_tk_widget().pack(side=TOP, anchor=NW, expand=1)
        else:
            report_text.configure(text="Venn diagrams can only be generated for up to 6 studies.", font=("Calibri bold", 12), fg='red')
        # Create the buttons and add them to the button frame
        export_table_button = tk.Button(master=button_frame, text="Export\nTable", height=2, width=7, relief='ridge', font=("Calibri", 10), background='white', command=lambda: exporterer(overlap_results))
        export_table_button.pack(side=LEFT, anchor=NW)
        export_table_button.config(activebackground='white')
        if len(lists) <=6:
            export_venn_button = tk.Button(master=button_frame, text="Export\nVenn", height=2, width=7, relief='ridge', font=("Calibri", 10), background='white', command=lambda: overlap_venn(lists).savefig(fname=tk.filedialog.asksaveasfilename(defaultextension=".png", filetypes=(("Portable Network Graphics Format", "*.png"), ("Joint Photographic Experts Group", ".jpeg"), ("Portable Document Format", ".pdf"), ("Encapsulated Postscript", ".eps"), ("Postscript", ".ps"), ("Scalable Vector Graphics", ".svg"), ("Scalable Vector Graphics", ".svgz"), ("Tagged Image File Format", ".tiff"), ("All Files", "*.*"))), dpi=1000))
            export_venn_button.pack(side=LEFT, anchor=NW)
            export_venn_button.config(activebackground='white')
def find_protein(genename= "", exact=True):
    global fam_check
    studies_found = list()
    genename=genename.strip("\n")
    if genename !="":
        for study in retrieve_col(retrieve="code_name"):
            if exact == True:
                fam_check=False
                if genename.upper() in retrieve_results(study):
                    studies_found.append(retrieve_col(retrieve="written_name", filter="code_name", select=study)[0])
            if exact == False:
                fam_check=True
                protein_list = list()
                for protein in retrieve_results(study):
                    if genename.upper() in protein.upper():
                        protein_list.append(protein)
                if protein_list:
                    studies_found.append(str(retrieve_col(retrieve="written_name", filter="code_name", select=study)[0])+"\t"+str(";".join(protein_list))+"\n")
        if studies_found:
            return(str("\n".join(studies_found)))
        else:
            return(str("Found in no studies!"))
def top_search(omic_string):
    omic_list= {omic_val for omic_val in omic_string.split("\n") if omic_val!=""}
    if len(omic_list)>1:
        proteins = dict()
        for study in omic_list:
            for protein in retrieve_results(retrieve_col(retrieve="code_name", filter="written_name", select=study)[0]):
                if protein in proteins:
                    proteins[protein]=proteins[protein] + 1
                elif protein not in proteins:
                    proteins[protein] = 1
        protein_list = list()
        for key, value in proteins.items():
            protein_list.append(str(value) +"\t"+ key)
        result_proteins = str()
        rank=1
        rank_temp=int(max(proteins.values()))
        result_proteins = [["Rank", "Rate", "Gene"]]
        for protein in sorted(protein_list, reverse=True, key=lambda line: int(line.split("\t")[0])):
            if int(protein.split("\t")[0])<rank_temp:    
                rank+=1
                rank_temp=int(protein.split("\t")[0])
            result_proteins.append([str(rank), protein.split("\t")[0], protein.split("\t")[1]])
        # Make the window
        result_window=tk.Tk()
        result_window.configure(background='white')
        result_window.geometry("250x600+"+str(root.winfo_x()-250)+"+"+str(root.winfo_y()-100))
        result_window.title("Damage Net v"+vers)
        result_window.wm_iconbitmap(iconfile)
        topsep = ttk.Separator(result_window, orient=HORIZONTAL)
        topsep.pack(side=TOP, fill=X)
        # Create the frame
        result_frame = tk.Frame(master=result_window, bg='white')
        result_frame.pack(fill=BOTH, expand=1)
        # Create the export button
        export_button = tk.Button(result_frame, text="Export", background='white', relief='ridge', font=("Calibri",12), command=lambda: exporterer(result_proteins))
        export_button.pack(side=TOP, anchor=NW)
        export_button.config(activebackground='white')
        # Create the popup window
        pop_window=tk.Tk()
        pop_window.configure(background='white')
        pop_window.geometry(str("300x100+"+str(result_window.winfo_x()+result_window.winfo_width()+10)+"+"+str(result_window.winfo_y())))
        pop_window.resizable(False, False)
        pop_window.update_idletasks()
        pop_window.overrideredirect(True)
        close_button=tk.Button(pop_window, text="Close", font=("Calibri", 10), relief='ridge', background='white', command=lambda: [text_label.config(state="normal"), text_label.delete("1.0", tk.END), pop_window.withdraw()])
        close_button.config(activebackground='white')
        close_button.pack(side=TOP, anchor=S)
        text_label=tk.Text(pop_window, background='white', font=("Calibri", 10))
        text_label.pack(side=TOP, anchor=N)
        pop_window.withdraw()
        # Create the table
        yscroll=Scrollbar(result_frame, orient="vertical")
        yscroll.pack(side=RIGHT, fill=Y)
        result_mlb=MultiListbox(result_frame)
        result_mlb.pack(side=TOP, anchor=NW, fill=BOTH, expand=1)
        result_mlb.focus_set()
        result_mlb.configure(columns=result_proteins[0], font=("Calibri",12), yscrollcommand=yscroll.set, selectbackground='#a9f5e8', selectforeground='black', headerfont=("Calibri bold",12), width=100, command=lambda event: [pop_window.deiconify(), text_label.config(state="normal"), text_label.insert("1.0", popup_info(omic_list, gene=str(result_mlb.get(event)[-1][-1]))), text_label.config(state=DISABLED)])
        for protein in result_proteins[1:]:
            result_mlb.insert(tk.END, *protein)       
        yscroll.config(command=result_mlb.yview)
        result_window.protocol("WM_DELETE_WINDOW", lambda: [pop_window.destroy(), result_window.destroy()])
def plot_mut_hm(mutdf):
    close()
    rcParams.update({'font.size':7, 'font.weight':'bold'})
    grid_kws = {"height_ratios": (.02, .9), "hspace": .2}
    fig, (ax1, ax2) = subplots(2, gridspec_kw=grid_kws)
    fig.set_size_inches(1,5)
    gcf().subplots_adjust(left=0.5)
    subplots_adjust(top=0.98)
    ax2=snsheatmap(mutdf, ax=ax2, cbar_ax=ax1, cmap= 'RdBu_r', vmin=-3.5, vmax=3.5, linecolor='black', linewidths=0.2, cbar_kws={"ticks":[-3,0,3], "orientation": "horizontal"})
    ax2.set_ylabel('')
    return(figure(get_fignums()[-1]))
def plot_km(gene, tcga):
    surv_conn=connect("database/Damage_Net_Survivalv1.0.db")
    expr_conn = connect("database/Damage_Net_TCGAv1.0.db")
    surv_data = surv_conn.execute("SELECT * FROM "+tcga).fetchall()
    expr_conn.row_factory = Row
    e = expr_conn.execute("SELECT * FROM "+tcga+" WHERE gene='"+gene.upper()+"'")
    expr_data = e.fetchone()
    cols = expr_data.keys()
    surv_conn.close()
    expr_conn.close()

    surv_expr=[[float(row[1]), float(row[2])]+[expr_data[row[0]]] for row in surv_data if row[0] in cols]
    surv_df = DataFrame(surv_expr, columns=["time", "status", "group"])

    tim = surv_df['time']
    obs = surv_df['status']
    gro = surv_df['group']
    ix = (gro == '0')
    close()
    rcParams.update({'font.size':7, 'font.weight':'bold'})
    figure(figsize=(3.5,3.5))
    subplots_adjust(top=1, right=1)
    kmf=KaplanMeierFitter()
    temp=kmf.fit(tim[~ix], obs[~ix], label='High')
    ax = kmf.plot(color='red')
    temp=kmf.fit(tim[ix], obs[ix], label='Low')
    ax = kmf.plot(ax=ax, color='blue')

    return(figure(get_fignums()[-1]))
def cancer_analysis(gene):
    result_window=tk.Tk()
    result_window.configure(background='white')
    result_window.title("Damage Net v"+vers)
    result_window.wm_iconbitmap(iconfile)
    result_window.geometry("+"+str(root.winfo_x()+200)+"+"+str(root.winfo_y()+31))

    load_label=tk.Label(result_window, text="Loading:\t (0%)", font=("Calibri",12), bg='white')
    load_label.grid(row=0, column=0, sticky=NSEW)
    progress = ttk.Progressbar(result_window, orient=HORIZONTAL, length=300)
    progress.grid(row=1, column=0)
    result_window.update()

    val=0
    progress['value'] = val
    load_label.configure(text=("Analysing ("+str(val)+"%)"))



    surv_conn=connect("database/Damage_Net_Survivalv1.0.db")
    s = surv_conn.cursor()
    expr_conn = connect("database/Damage_Net_TCGAv1.0.db")
    c = expr_conn.cursor()
    TCGA_db = ["BRCA", "CHOL", "GBM", "LGG", "CESC", "COAD",  "KIRP",  "LIHC",  "LUAD",  "LUSC",  "MESO",  "PAAD", "PRAD", "SARC", "STAD", "THCA", "UCS", "LAML", "ACC", "BLCA", "ESCA", "HNSC", "KICH", "KIRC", "DLBC", "OV", "PCPG", "READ", "SKCM", "TGCT", "THYM", "UCEC", "UVM"]

    genes = {gen[0] for gen in c.execute("SELECT gene FROM ACC").fetchall()}
    if gene.upper() in genes:

        lfc_tab = list()
        surv_tab=list()
        for tcga in sorted(TCGA_db):
            exprs = c.execute("SELECT * FROM "+tcga+" WHERE gene='"+gene.upper()+"'").fetchall()[0][1:]
            pms = c.execute("SELECT * FROM "+tcga+" WHERE gene='Point_Mutation_Total'").fetchall()[0][1:]
            inss = c.execute("SELECT * FROM "+tcga+" WHERE gene='Insertions'").fetchall()[0][1:]
            delss = c.execute("SELECT * FROM "+tcga+" WHERE gene='Deletions'").fetchall()[0][1:]
            pml=list()
            pmh=list()
            insl=list()
            insh=list()
            dell=list()
            delh=list()
            for expr,pm,ins,dels in zip(exprs, pms, inss, delss):
                if expr=="0":
                    pml.append(int(pm))
                    insl.append(int(ins))
                    dell.append(int(dels))
                elif expr=="1":
                    pmh.append(int(pm))
                    insh.append(int(ins))
                    delh.append(int(dels))
            lfc_tab.append([tcga, log2(average(pmh)/average(pml)), log2(average(insh)/average(insl)), log2(average(delh)/average(dell))])

            surv_data = s.execute("SELECT * FROM "+tcga).fetchall()
            expr_conn.row_factory = Row
            expr_data = expr_conn.execute("SELECT * FROM "+tcga+" WHERE gene='"+gene.upper()+"'").fetchone()
            cols = expr_data.keys()

            surv_expr=[[float(row[1]), float(row[2])]+[expr_data[row[0]]] for row in surv_data if row[0] in cols]
            surv_df = DataFrame(surv_expr, columns=["time", "status", "group"])
            surv_cph = CoxPHFitter().fit(surv_df, 'time', 'status')
            cph_out = [str(tcga), (-1)*log2(float(surv_cph.summary['exp(coef)'])), float(surv_cph.summary['p'])]
            surv_tab.append(cph_out)

            val+=3
            progress['value'] = val
            load_label.configure(text=("Analysing ("+str(val)+"%)"))
            result_window.update_idletasks()

        progress['value'] = 100
        load_label.configure(text=("Analysing ("+str(val)+"%)"))
        result_window.update_idletasks()

        surv_conn.close()
        expr_conn.close()
        mut_df = DataFrame(lfc_tab, columns=["TCGA", "PM", "Ins", "Del"]).set_index('TCGA')
        out_tab = [["Subtype", "Point Mutations", "Insertions", "Deletions", "Survival Coefficient", "Survival P.value"]]+[mut+surv[1:] for mut, surv in zip(lfc_tab, surv_tab)]
        progress.destroy()
        load_label.destroy()
        result_window.geometry("+"+str(root.winfo_x()+200)+"+"+str(root.winfo_y()))
        topsep = ttk.Separator(result_window, orient=HORIZONTAL)
        topsep.pack(side=TOP, fill=X)

        hm_frame=tk.Frame(master=result_window, bg='white')
        hm_frame.pack(side=LEFT, anchor=NW, expand=1)
        hm_label = tk.Label(hm_frame, text=(" Mutation associations  \n of "+gene.upper()+":"), font=("Calibri",10), bg='white', justify=LEFT)
        hm_label.pack(side=TOP, anchor=NW, expand=1)
        mut_hm = plot_mut_hm(mut_df)
        mut_hm_fig = FigureCanvasTkAgg(mut_hm, hm_frame)
        mut_hm_fig.get_tk_widget().pack(side=TOP, anchor=NW, expand=1)
        topsep = ttk.Separator(result_window, orient=VERTICAL)
        topsep.pack(side=LEFT, fill=Y)

        surv_frame=tk.Frame(master=result_window, bg='white')
        surv_frame.pack(side=LEFT, anchor=NW, expand=1)
        surv_label = tk.Label(surv_frame, text="Survival analysis for "+gene.upper()+":", font=("Calibri",10), bg='white', justify=LEFT)
        surv_label.pack(side=TOP, anchor=NW, expand=1)
        tab_frame=tk.Frame(master=surv_frame, bg='white')
        tab_frame.pack(side=TOP, anchor=NW, expand=1)

        for r in range(0,2):
            surv_label = tk.Label(tab_frame, text="Surv.Coef", font=("Calibri",8), bg='white', borderwidth=1, relief='solid')
            surv_label.grid(column=0, row=1+(r*4), sticky='nsew')
            p_label = tk.Label(tab_frame, text="P.Value", font=("Calibri",8), bg='white', borderwidth=1, relief='solid')
            p_label.grid(column=0, row=2+(r*4), sticky='nsew')
        for col in range(0,len(surv_tab)):
            ccol='white'
            col2=col
            if surv_tab[col][2] < 0.05:
                ccol = '#a9f5e8'
            if col==16:
                label_temp = tk.Label(tab_frame, text="", bg='white', height=1)
                label_temp.grid(column=7, row=3, sticky='nsew')
            if col > 16:
                col2=col-17
            for row in range(0,3):
                fonts=7
                if row == 2:
                    if surv_tab[col][row] < 0.01:
                        value = "<0.01"
                    else:
                        value = round(surv_tab[col][row], 2)
                elif row ==1:
                    value = round(surv_tab[col][row], 2)
                else:
                    fonts=8
                    value = surv_tab[col][row]
                if col > 16:
                    row=row+4
                label_temp = tk.Label(tab_frame, text=value, font=("Calibri",fonts), bg=ccol, borderwidth=1, relief='solid')
                label_temp.grid(column=col2+1, row=row, sticky='nsew')

        label_temp = tk.Label(surv_frame, text="", bg='white', height=1)
        label_temp.pack(side=TOP, anchor=NW, expand=1)
        km_frame=tk.Frame(master=surv_frame, bg='white')
        km_frame.pack(side=TOP, anchor=NW, expand=1)
        kmp_frame=tk.Frame(master=km_frame, bg='white')
        kmp_frame.pack(side=LEFT, anchor=NW, expand=1)
        km_label = tk.Label(kmp_frame, text="Kaplan-Meier curve for "+gene.upper()+" expression in BRCA:", font=("Calibri",10), bg='white')
        km_label.pack(side=TOP, anchor=NW, expand=1)
        km_plot = plot_km(gene.upper(), "BRCA")
        km_plot_fig = FigureCanvasTkAgg(km_plot, kmp_frame)
        km_plot_widg = km_plot_fig.get_tk_widget()
        km_plot_widg.pack(side=LEFT, anchor=NW, expand=1)

        label_temp = tk.Label(km_frame, text="", bg='white', width=1)
        label_temp.pack(side=LEFT, anchor=NW, expand=1)
        next_frame=tk.Frame(master=km_frame, bg='white')
        next_frame.pack(side=LEFT, anchor=NW, expand=1)
        label_temp = tk.Label(next_frame, text="", bg='white', height=1)
        label_temp.pack(side=TOP, anchor=NW, expand=1)
        butt_frame=tk.Frame(master=next_frame, bg='white')
        butt_frame.pack(side=TOP, anchor=NW, expand=1)
        export_button = tk.Button(butt_frame, text="Export results table", width=20, background='white', relief='ridge', font=("Calibri",10), command = lambda: [exporterer(out_tab), result_window.lift()])
        export_button.pack(side=TOP, anchor=NW)
        export_button.config(activebackground='white')
        kmex_button = tk.Button(butt_frame, text="Export\nKM-plot", width=9, background='white', relief='ridge', font=("Calibri",10), command = lambda: [plot_km(gene.upper(), km_drop_str.get().strip("\n")).savefig(fname=tk.filedialog.asksaveasfilename(defaultextension=".png", filetypes=(("Portable Network Graphics Format", "*.png"), ("Joint Photographic Experts Group", ".jpeg"), ("Portable Document Format", ".pdf"), ("Encapsulated Postscript", ".eps"), ("Postscript", ".ps"), ("Scalable Vector Graphics", ".svg"), ("Scalable Vector Graphics", ".svgz"), ("Tagged Image File Format", ".tiff"), ("All Files", "*.*"))), dpi=500), result_window.lift()])
        kmex_button.pack(side=LEFT, anchor=NW)
        kmex_button.config(activebackground='white')
        hmex_button = tk.Button(butt_frame, text="Export\nheatmap", width=9, background='white', relief='ridge', font=("Calibri",10), command = lambda: [plot_mut_hm(mut_df).savefig(fname=tk.filedialog.asksaveasfilename(defaultextension=".png", filetypes=(("Portable Network Graphics Format", "*.png"), ("Joint Photographic Experts Group", ".jpeg"), ("Portable Document Format", ".pdf"), ("Encapsulated Postscript", ".eps"), ("Postscript", ".ps"), ("Scalable Vector Graphics", ".svg"), ("Scalable Vector Graphics", ".svgz"), ("Tagged Image File Format", ".tiff"), ("All Files", "*.*"))), dpi=500), result_window.lift()])
        hmex_button.pack(side=RIGHT, anchor=NE)
        hmex_button.config(activebackground='white')
        drop_label = tk.Label(next_frame, text="Select sub-type for KM-plot:", font=("Calibri",10), bg='white')
        drop_label.pack(side=TOP, anchor=NW)
        km_drop_str=tk.StringVar(next_frame)
        km_drop_str.set("BRCA")
        km_drop=tk.OptionMenu(next_frame, km_drop_str, *sorted(TCGA_db), command = lambda x: [[widg.destroy() for widg in kmp_frame.winfo_children() if 'label' not in str(widg)], FigureCanvasTkAgg(plot_km(gene.upper(), x), kmp_frame).get_tk_widget().pack(side=LEFT, anchor=NW, expand=1), km_label.configure(text="Kaplan-Meier curve for "+gene.upper()+" expression in "+x+":")])
        km_drop.config(width=16, height=1, relief='solid', borderwidth=1, background='white', font=("Calibri",10), highlightbackground='white', activebackground='white')
        km_drop["menu"].config(bg="white", activebackground='#a9f5e8', activeforeground='black')
        km_drop.pack(side=TOP, anchor=NW)



    else:
        progress.destroy()
        load_label.configure(fg='red', text="ERROR: Gene name not found.\nTry using another version")



# GUI functions
def output_copy():
    if output_text1.get('1.0', END).strip("\n") not in ["Found in no studies!", ""]:
        output_text2.delete("1.0", tk.END)
        if fam_check:
            for line in output_text1.get("1.0", tk.END).split("\n"):
                output_text2.insert(tk.END, line.split("\t")[0]+"\n")
        elif not fam_check:
            output_text2.insert(tk.END, output_text1.get("1.0", tk.END))
def study_select(event):
    if event != "<EMPTY>":
        if event not in output_text2.get("1.0",END):
            output_text2.insert(tk.END, str(event+"\n"))
    strvar_list = [dmgchr_ass_str, interactomes_str, dmg_mods_str, genetic_screens_str, custom_1_str, custom_2_str]
    key_list =  ['chr_assoc', 'inters', 'mods', 'gen_scre', 'custom_1', 'custom_2']
    for strvar,key in zip(strvar_list, key_list):
        if len(category_dict[key]) <=15:
            strvar.set(" "+category_dict[key]+"\t\t")
        elif len(category_dict[key]) <=20:
            strvar.set(" "+category_dict[key]+"\t")
        else:
            strvar.set(category_dict[key])
def keep_flat(event):
    if event.widget in (compare, topsearch, searchbutton, cancer, export_button, clear_button, dmgchr_ass_menu, interactomes_menu, dmg_mods_menu, genetic_screens_menu):
        event.widget.config(relief=FLAT)
def focus_next(event):
    event.widget.tk_focusNext().focus()
    return("break")
def focus_prev(event):
    event.widget.tk_focusPrev().focus()
    return("break")
def popup_info(studies, gene=""):
    full_studies=find_protein(genename=gene)
    cut_studies=[study for study in full_studies.split("\n") if study in studies]
    label_text=str(gene+":\n"+"\n".join(cut_studies)+"\n\n")
    return(label_text)
def option_select(event):
    option_drop_str.set("Options")
    def Add_dataset(tabl="dnet_1"):
        def dataset_retcon():
            entry_list=[name_entry, title_entry, doi_entry, method_entry, results_entry]
            complete=True
            for entry in entry_list:
                if len(entry.get("1.0", tk.END).strip("\n"))==0:
                    report_text.configure(text="Complete entry fields!", fg='red')
                    complete=False
            if len(category_str.get().strip("\n"))=="Select category":
                report_text.configure(text="Select a category!", fg='red')
                complete=False
            if complete:
                menu_dict={"Chromatin Associated":dmgchr_ass_menu, "Interactomes":interactomes_menu, "Modifications":dmg_mods_menu, "Genetic Screens":genetic_screens_menu, category_dict['custom_1']:custom_1_menu, category_dict['custom_2']:custom_2_menu}
                if re.sub(" ", "_", name_entry.get("1.0", tk.END)).strip("\n") not in (retrieve_col(retrieve="code_name")) and name_entry.get("1.0", tk.END).strip("\n") not in (retrieve_col(retrieve="written_name")):
                    name=name_entry.get("1.0", tk.END).strip("\n")
                    menu_dict[category_str.get().strip("\n")]['menu'].add_command(label=name, command=lambda: study_select(name))
                    insert_data(list((re.sub(" ", "_", name_entry.get("1.0", tk.END)).strip("\n"), name_entry.get("1.0", tk.END).strip("\n"), category_str.get().strip("\n"), title_entry.get("1.0", tk.END).strip("\n"), doi_entry.get("1.0", tk.END).strip("\n"), method_entry.get("1.0", tk.END).strip("\n"), results_entry.get("1.0", tk.END).strip("\n"))))
                    report_text.configure(text="Dataset added!", fg='green')
                    if menu_dict[category_str.get().strip("\n")]['menu'].entrycget(0, "label") == "<EMPTY>":
                         menu_dict[category_str.get().strip("\n")]['menu'].delete("<EMPTY>")
                else:
                    report_text.configure(text="Name already in use!", fg='red')
        # Create the input window
        input_window=tk.Tk()
        input_window.title("Add Dataset")
        input_window.wm_iconbitmap(iconfile)
        input_window.resizable(False, False)
        input_window.configure(background='white')
        input_window.geometry(str("+"+str(root.winfo_x()+root.winfo_width()-500)+"+"+str(root.winfo_y()+100)))
        input_window.update_idletasks()
        # Fill with the input boxes
        # Input name
        name_label=tk.Label(input_window, text="Dataset name:", font=("Calibri",12), bg='white')
        name_label.grid(row=0, column=0, sticky=W)
        name_entry=tk.Text(input_window, width=25, height=1, font=("Calibri",10))
        name_entry.grid(row=0, column=1)
        name_entry.bind('<Tab>', focus_next)
        name_entry.bind('<Shift-Tab>', focus_prev)
        # Category input
        category_label=tk.Label(input_window, text="Dataset Category:       ", font=("Calibri",12), bg='white')
        category_label.grid(row=1, column=0, sticky=W)
        category_str=tk.StringVar(input_window)
        category_str.set("Select category")
        category_entry=tk.OptionMenu(input_window, category_str, *list(OrderedDict().fromkeys(retrieve_col(retrieve="category")+list(category_dict.values()))))
        category_entry.config(width=20, height=1, relief='flat', background='white', font=("Calibri",10), highlightbackground='white', activebackground='white')
        category_entry["menu"].config(bg="white", activebackground='#a9f5e8', activeforeground='black')
        category_entry.grid(row=1, column=1)
        category_entry.bind('<Tab>', focus_next)
        category_entry.bind('<Shift-Tab>', focus_prev)
        # Publication title input
        title_label=tk.Label(input_window, text="Publication title:", font=("Calibri",12), bg='white')
        title_label.grid(row=2, column=0, sticky=W)
        title_entry=tk.Text(input_window, width=25, height=1, font=("Calibri",10))
        title_entry.grid(row=2, column=1)
        title_entry.bind('<Tab>', focus_next)
        title_entry.bind('<Shift-Tab>', focus_prev)
        # Publication doi input
        doi_label=tk.Label(input_window, text="Publication DOI:", font=("Calibri",12), bg='white')
        doi_label.grid(row=3, column=0, sticky=W)
        doi_entry=tk.Text(input_window, width=25, height=1, font=("Calibri",10))
        doi_entry.grid(row=3, column=1)
        doi_entry.bind('<Tab>', focus_next)
        doi_entry.bind('<Shift-Tab>', focus_prev)
        # Method input
        method_label=tk.Label(input_window, text="Methodology:", font=("Calibri",12), bg='white')
        method_label.grid(row=4, column=0, sticky=W)
        method_entry=tk.Text(input_window, width=25, height=1, font=("Calibri",10))
        method_entry.grid(row=4, column=1)
        method_entry.bind('<Tab>', focus_next)
        method_entry.bind('<Shift-Tab>', focus_prev)
        # Results input
        results_label=tk.Label(input_window, text="Results:", font=("Calibri",12), bg='white')
        results_label.grid(row=5, column=0, sticky=W)
        results_entry=tk.Text(input_window, width=25, height=1, font=("Calibri",10))
        results_entry.grid(row=5, column=1)
        results_entry.bind('<Tab>', focus_next)
        results_entry.bind('<Shift-Tab>', focus_prev)
        # Import table button
        import_result_button=tk.Button(input_window, text="Import Results Table", font=("Calibri", 10), padx=30, relief='ridge', background='white', command=lambda: [results_entry.delete("1.0", tk.END), results_entry.insert(tk.END, ";".join(importation(tk.filedialog.askopenfilename(filetypes=(("All Files", "*.*"), ("Excel Workbook", "*.xlsx"), ("Excel 97-2003 Workbook", "*.xls"), ("Comma delimited", ".csv"), ("Tab delimited", ".tsv"), ("Text (Tab delimited)", ".txt")))))), input_window.lift()])
        import_result_button.config(activebackground='white')
        import_result_button.grid(row=6, column=1)
        import_result_button.bind('<Tab>', focus_next)
        import_result_button.bind('<Shift-Tab>', focus_prev)
        report_text=tk.Label(input_window, text="", font=("Calibri",12), bg='white')
        report_text.grid(row=7, column=0)
        save_button=tk.Button(input_window, text="Save Dataset to Database", font=("Calibri", 10), padx=17, relief='ridge', background='white', command=dataset_retcon)
        save_button.config(activebackground='white')
        save_button.grid(row=7, column=1)
        save_button.bind('<Tab>', focus_next)
        save_button.bind('<Shift-Tab>', focus_prev)
        topsep = ttk.Separator(input_window, orient=HORIZONTAL)
        topsep.grid(row=0, column=0, columnspan=2, sticky="new")
    def Remove_dataset():
        def dataset_drop(event):
            dataset_str.set("Select dataset")
            dataset_entry['menu'].delete("0", tk.END)
            if not retrieve_col(retrieve="written_name", filter="category", select=event.strip("\n")):
                dataset_entry['menu'].add_command(label="<EMPTY>")
            for dataset in retrieve_col(retrieve="written_name", filter="category", select=event.strip("\n")):
                dataset_entry['menu'].add_command(label=dataset, command=lambda: dataset_str.set(dataset))
        def dataset_retcon():
            if category_str.get().strip("\n")=="Select category":
                report_text.configure(text="Select category!", fg='red')
            elif dataset_str.get().strip("\n")=="Select dataset":
                report_text.configure(text="Select dataset!", fg='red')
            else:
                data_query("DELETE FROM dnet_1 WHERE code_name='"+retrieve_col(retrieve="code_name", filter="written_name", select=dataset_str.get().strip("\n"))[0]+"'")
                menu_dict={"Chromatin Associated":dmgchr_ass_menu, "Interactomes":interactomes_menu, "Modifications":dmg_mods_menu, "Genetic Screens":genetic_screens_menu, category_dict['custom_1']:custom_1_menu, category_dict['custom_2']:custom_2_menu} 
                menu_dict[category_str.get().strip("\n")]['menu'].delete(dataset_str.get().strip("\n"))
                report_text.configure(text="Dataset removed!", fg='green')
                dataset_drop(category_str.get().strip("\n"))
                if menu_dict[category_str.get().strip("\n")]['menu'].index(tk.END) == None:
                    menu_dict[category_str.get().strip("\n")]['menu'].add_command(label="<EMPTY>")
        # Create the input window 
        input_window=tk.Tk()
        input_window.title("Remove dataset")
        input_window.wm_iconbitmap(iconfile)
        input_window.resizable(False, False)
        input_window.configure(background='white')
        input_window.geometry(str("+"+str(root.winfo_x()+root.winfo_width()-500)+"+"+str(root.winfo_y()+100)))
        input_window.update_idletasks()
        # Category dropdown
        category_label=tk.Label(input_window, text="Select category:           ", font=("Calibri",12), bg='white')
        category_label.grid(row=0, column=0, sticky=W)
        category_str=tk.StringVar(input_window)
        category_str.set("Select category")
        category_entry=tk.OptionMenu(input_window, category_str, *list(OrderedDict().fromkeys(retrieve_col(retrieve="category")+list(category_dict.values()))), command=lambda event: dataset_drop(event))
        category_entry.config(width=20, height=1, relief='flat', background='white', font=("Calibri",10), highlightbackground='white', activebackground='white')
        category_entry["menu"].config(bg="white", activebackground='#a9f5e8', activeforeground='black')
        category_entry.grid(row=0, column=1)
        # Dataset dropdown
        dataset_label=tk.Label(input_window, text="Select dataset:", font=("Calibri",12), bg='white')
        dataset_label.grid(row=1, column=0, sticky=W)
        dataset_str=tk.StringVar(input_window)
        dataset_str.set("Select category first")
        dataset_entry=tk.OptionMenu(input_window, dataset_str, *["Select category first"])
        dataset_entry.config(width=20, height=1, relief='flat', background='white', font=("Calibri",10), highlightbackground='white', activebackground='white')
        dataset_entry["menu"].config(bg="white", activebackground='#a9f5e8', activeforeground='black')
        dataset_entry.grid(row=1, column=1)
        # Remove button
        report_text=tk.Label(input_window, text="", font=("Calibri",12), bg='white')
        report_text.grid(row=2, column=0)
        remove_button=tk.Button(input_window, text="Remove dataset", font=("Calibri", 10), padx=43, relief='ridge', background='white', command=dataset_retcon)
        remove_button.config(activebackground='white')
        remove_button.grid(row=2, column=1)
        topsep = ttk.Separator(input_window, orient=HORIZONTAL)
        topsep.grid(row=0, column=0, columnspan=2, sticky="new")
    def Rename_category():
        def category_retcon():
            new_name=rename_entry.get().strip("\n")
            if category_str.get().strip("\n") == "Select category":
                report_text.configure(text="Select a category!", fg='red')
            elif new_name=="":
                report_text.configure(text="Enter name", fg='red')
            elif new_name in list(category_dict.values()):
                report_text.configure(text="Name already in use!", fg='red')
            elif new_name not in (list(category_dict.values())+[""]) and category_str.get().strip("\n") != "Select category": 
                update_settings("category_dict", category_str.get().strip("\n"), new_name)
                study_select("<EMPTY>")
                rename_category(old=category_str.get().strip("\n"), new=new_name)
                category_entry['menu'].delete(category_str.get().strip("\n"))
                category_entry['menu'].add_command(label=new_name, command=lambda: category_str.set(new_name))
                category_str.set(new_name)
                report_text.configure(text="Rename successful!", fg='green')
        # Create the input window
        input_window=tk.Tk()
        input_window.title("Rename category")
        input_window.wm_iconbitmap(iconfile)
        input_window.resizable(False, False)
        input_window.configure(background='white')
        input_window.geometry(str("+"+str(root.winfo_x()+root.winfo_width()-500)+"+"+str(root.winfo_y()+100)))
        input_window.update_idletasks()
        # Category dropdown
        category_label=tk.Label(input_window, text="Select category:         ", font=("Calibri",12), bg='white')
        category_label.grid(row=0, column=0, sticky=W)
        rename_entry=tk.Entry(input_window, width=25, font=("Calibri",10))
        category_str=tk.StringVar(input_window)
        category_str.set("Select category")
        category_entry=tk.OptionMenu(input_window, category_str, *category_dict.values(), command=lambda event:[rename_entry.delete(0, tk.END), rename_entry.insert(tk.END, event)])
        category_entry.config(width=20, height=1, relief='flat', background='white', font=("Calibri",10), highlightbackground='white', activebackground='white')
        category_entry["menu"].config(bg="white", activebackground='#a9f5e8', activeforeground='black')
        category_entry.grid(row=0, column=1)
        # New name input
        rename_label=tk.Label(input_window, text="New name:", font=("Calibri",12), bg='white')
        rename_label.grid(row=1, column=0, sticky=W)
        rename_entry.bind('<Return>', lambda event: category_retcon())
        rename_entry.grid(row=1, column=1)
        # Save button
        report_text=tk.Label(input_window, text="", font=("Calibri",12), bg='white')
        report_text.grid(row=2, column=0)
        save_button=tk.Button(input_window, text="Rename category", font=("Calibri", 10), padx=39, relief='ridge', background='white', command=category_retcon)
        save_button.config(activebackground='white')
        save_button.grid(row=2, column=1)
        topsep = ttk.Separator(input_window, orient=HORIZONTAL)
        topsep.grid(row=0, column=0, columnspan=2, sticky="new")
    def View_datasets():
        data_table=[["Name", "Category", "Method", "Publication", "DOI", "Results"]]
        for dataset in retrieve_col(retrieve="code_name"):
            name=retrieve_col(retrieve="written_name", filter="code_name", select=dataset)[0]
            category=retrieve_col(retrieve="category", filter="code_name", select=dataset)[0]
            method=retrieve_col(retrieve="method", filter="code_name", select=dataset)[0]
            publication=retrieve_col(retrieve="title", filter="code_name", select=dataset)[0]
            doi=retrieve_col(retrieve="DOI", filter="code_name", select=dataset)[0]
            results=retrieve_results(dataset)
            data_table.append([name, category, method, publication, doi, ";".join(results)])
        # Make the window
        data_window=tk.Tk()
        data_window.configure(background='white')
        data_window.title("Data View")
        data_window.wm_iconbitmap(iconfile)
        data_window.geometry("800x400+"+str(root.winfo_x()+200)+"+"+str(root.winfo_y()+31))
        topsep = ttk.Separator(data_window, orient=HORIZONTAL)
        topsep.pack(side=TOP, fill=X)
        # Make and pack the frames
        button_frame=tk.Frame(master=data_window, bg='white')
        button_frame.pack(side=TOP, anchor=NW)
        table_frame = tk.Frame(master=data_window, bg='white')
        table_frame.pack(side=TOP, fill=BOTH, expand=1)
        # Create the table and add it to the table frame    
        yscroll=Scrollbar(table_frame, orient="vertical")
        yscroll.pack(side=RIGHT, fill=Y)
        xscroll=Scrollbar(table_frame, orient="horizontal")
        result_mlb=MultiListbox(table_frame)
        result_mlb.pack(side=TOP, anchor=NW, fill=BOTH, expand=1)
        xscroll.pack(side=TOP, fill=X)
        result_mlb.focus_set()
        result_mlb.configure(columns=data_table[0], font=("Calibri",10), yscrollcommand=yscroll.set, xscrollcommand=xscroll.set, xscrollincrement=1, selectbackground='#a9f5e8', selectforeground='black', headerfont=("Calibri bold",12), width=100)
        for dataset in data_table[1:]:
            result_mlb.insert(tk.END, *dataset)       
        yscroll.config(command=result_mlb.yview)
        xscroll.config(command=result_mlb.xview)
        # Create export button
        export_table_button = tk.Button(master=button_frame, text="Export\nTable", height=2, width=7, relief='ridge', font=("Calibri", 10), background='white', command=lambda: exporterer(data_table))
        export_table_button.pack(side=LEFT, anchor=NW)
        export_table_button.config(activebackground='white')
    def Help():
        # Create the input window
        input_window=tk.Tk()
        input_window.title("Help")
        input_window.wm_iconbitmap(iconfile)
        input_window.resizable(False, False)
        input_window.configure(background='white')
        input_window.geometry(str("+"+str(root.winfo_x()+root.winfo_width()-500)+"+"+str(root.winfo_y()+100)))
        input_window.update_idletasks()
        # Just a label pointing to the GitHub right now...
        help_str="For further information\nand help on how to use Damage Net,\nplease refer to the GitHub page:\nhttps://github.com/aldob/Damage-Net"
        help_text=tk.Label(input_window, text=help_str, font=("Calibri", 12), background='white', padx=30, pady=30)
        help_text.pack(side=TOP, anchor=NW)
    locals()[re.sub(" ", "_", event)]()





# destroy the loading widgets and resize the window for the actual program
load_label.destroy()
logo_canv.destroy()
root.geometry("+500+300")

 







load_text.set("Loading graphical interface...")
root.update_idletasks()
# Create option dropdown
root.bind("<Button-1>", keep_flat)
option_list=["Add dataset", "Remove dataset", "Rename category", "View datasets", "Help"]
option_drop_str=tk.StringVar()
option_drop_str.set("Options")
option_drop = tk.OptionMenu(root, option_drop_str, *option_list, command=option_select)
option_drop.config(indicatoron=0, height=1, relief='flat', background='white', font=("Calibri",10), highlightbackground='white', activebackground='white')
option_drop.grid(row=0, column=0, sticky=NW)
option_drop["menu"].config(bg="white", activebackground='#a9f5e8', activeforeground='black')





# Create the find protein section
# Gene entry
gene_entry_frame = ttk.Frame(master=root, style="RoundedFrame", padding=10)
gene_entry = tk.Entry(gene_entry_frame, borderwidth=0, highlightthickness=0, width=25, font=("Calibri",12))
gene_entry.pack(fill="both", expand=True)
gene_entry.bind("<FocusIn>", lambda event: gene_entry_frame.state(["focus"]), add="+")
gene_entry.bind("<FocusIn>", lambda event: gene_entry.delete(0, tk.END) if gene_entry.get().strip("\n")=="Gene name" else None, add="+")
gene_entry.bind("<Return>", lambda event: [output_text1.delete("1.0", tk.END), output_text1.insert(tk.END, find_protein(genename=gene_entry.get(), exact=var1.get()))], add="+")
gene_entry.bind("<FocusOut>", lambda event: gene_entry_frame.state(["!focus"]), add="+")
gene_entry.bind("<FocusOut>", lambda event: gene_entry.insert(tk.END, "Gene name") if len(gene_entry.get().strip("\n"))==0 else None, add="+")
gene_entry.insert(tk.END, "Gene name")
gene_entry_frame.grid(row=1, column=0, stick=EW)
Grid.rowconfigure(root, 1, weight=1)
# Search button
searchimage = Image.open("images/searchicon.png")
searchimage = ImageTk.PhotoImage(searchimage.resize((26,25),Image.ANTIALIAS))
searchimage_act = Image.open("images/searchicon_active.png")
searchimage_act = ImageTk.PhotoImage(searchimage_act.resize((26,25),Image.ANTIALIAS))
searchbutton = tk.Button(root, image=searchimage, background='white', width=20, relief='flat', command=lambda: [output_text1.delete("1.0", tk.END), output_text1.insert(tk.END, find_protein(genename=gene_entry.get(), exact=var1.get()))])
searchbutton.bind("<Button-1>", lambda event: [searchbutton.configure(image=searchimage_act)])
searchbutton.bind("<ButtonRelease-1>", lambda event: [searchbutton.configure(image=searchimage)])
searchbutton.config(activebackground='white')
searchbutton.grid(row=1, column=1)
# Family search checkbox
var1 = tk.BooleanVar(value=1)
check=tk.Checkbutton(root, text="Family\nsearch", background='white', font=("Calibri",10), variable=var1, onvalue=False, offvalue=True)
check.config(activebackground='white')
check.grid(row=2, column=0, stick=W)
# Cancer analysis button
cancer_frame = ttk.Frame(master=root, style="RoundedFrame", padding=10)
cancer = tk.Button(cancer_frame, text="Pan-Cancer Mutation\nand Survival Analysis", font=("Calibri bold",12), background='white', relief='flat', command=lambda: cancer_analysis(gene_entry.get()))
cancer.pack(fill="both", expand=True)
cancer.bind("<Button-1>", lambda event: cancer_frame.state(["focus"]))
cancer.bind("<ButtonRelease-1>", lambda event: cancer_frame.state(["!focus"]))
cancer.config(activebackground='white')
cancer_frame.grid(row=3, column=0, rowspan=2, stick=W)
# Output text box
output_text1_frame = ttk.Frame(master=root, style="RoundedFrame", padding=10)
output_text1 = tk.Text(output_text1_frame, borderwidth=0, highlightthickness=0, wrap="word", height=10, width=45, font=("Calibri",12))
output_text1.grid(row=0, rowspan=2, column=0, columnspan=2, sticky=NSEW)
Grid.columnconfigure(output_text1_frame, 0, weight=1)
Grid.rowconfigure(output_text1_frame, 0, weight=1)
output_text1.bind("<FocusIn>", lambda event: output_text1_frame.state(["focus"]))
output_text1.bind("<FocusOut>", lambda event: output_text1_frame.state(["!focus"]))
output_text1_frame.grid(row=5, column=0, columnspan=3, sticky=N+S+E+W)
Grid.columnconfigure(root, 0, weight=1)
Grid.rowconfigure(root, 5, weight=1)
# Copy over button
export_frame = ttk.Frame(master=output_text1_frame, style="RoundedFrame", padding=10)
export_button = tk.Button(master=export_frame, text="Copy to >>", width=7, relief='flat', font=("Calibri", 10), background='white', command=output_copy)
export_button.pack()
export_button.bind("<Button-1>", lambda event: export_frame.state(["focus"]))
export_button.bind("<ButtonRelease-1>", lambda event: export_frame.state(["!focus"]))
export_button.config(activebackground='white')
export_frame.grid(row=1, column=1, sticky=SE)
fam_check=False





# Create the top search/overlap section
# Create dropdowns for dataset selection
drop_img = Image.open("images/t3.png")
drop_img = ImageTk.PhotoImage(drop_img.resize((175,26),Image.ANTIALIAS))

dmgchr_ass_str=tk.StringVar()
dmgchr_ass_list=sorted(retrieve_col(retrieve="written_name", filter="category", select=category_dict['chr_assoc']))
if not dmgchr_ass_list:
    dmgchr_ass_list.append("<EMPTY>")
dmgchr_ass_menu = tk.OptionMenu(root, dmgchr_ass_str, *dmgchr_ass_list, command=study_select)
dmgchr_ass_menu.config(indicatoron=0, height=12, relief='flat', background='white', font=("Calibri",11), highlightbackground='white', activebackground='white', compound=CENTER, image=drop_img)
dmgchr_ass_menu.grid(row=1, column=3, columnspan=2, sticky=NW)
dmgchr_ass_menu["menu"].config(bg="white", activebackground='#a9f5e8', activeforeground='black')

interactomes_str=tk.StringVar()
interactomes_list=sorted(retrieve_col(retrieve="written_name", filter="category", select=category_dict['inters']))
if not interactomes_list:
    interactomes_list.append("<EMPTY>")
interactomes_menu = tk.OptionMenu(root, interactomes_str, *interactomes_list, command=study_select)
interactomes_menu.config(indicatoron=0, height=12, relief='flat', background='white', font=("Calibri",11), highlightbackground='white', activebackground='white', compound=CENTER, image=drop_img)
interactomes_menu.grid(row=1, column=5, columnspan=2,  sticky=NW)
interactomes_menu["menu"].config(bg="white", activebackground='#a9f5e8', activeforeground='black')

dmg_mods_str=tk.StringVar()
dmg_mods_list=sorted(retrieve_col(retrieve="written_name", filter="category", select=category_dict['mods']))
if not dmg_mods_list:
    dmg_mods_list.append("<EMPTY>")
dmg_mods_menu = tk.OptionMenu(root, dmg_mods_str, *dmg_mods_list, command=study_select)
dmg_mods_menu.config(indicatoron=0, height=12, relief='flat', background='white', font=("Calibri",11), highlightbackground='white', activebackground='white', compound=CENTER, image=drop_img)
dmg_mods_menu.grid(row=2, column=3, columnspan=2,  sticky=NW)
dmg_mods_menu["menu"].config(bg="white", activebackground='#a9f5e8', activeforeground='black')

genetic_screens_str=tk.StringVar()
genetic_screens_list=sorted(retrieve_col(retrieve="written_name", filter="category", select=category_dict['gen_scre']))
if not genetic_screens_list:
    genetic_screens_list.append("<EMPTY>")
genetic_screens_menu = tk.OptionMenu(root, genetic_screens_str, *genetic_screens_list, command=study_select)
genetic_screens_menu.config(indicatoron=0, height=12, relief='flat', background='white', font=("Calibri",11), highlightbackground='white', activebackground='white', compound=CENTER, image=drop_img)
genetic_screens_menu.grid(row=2, column=5, columnspan=2,  sticky=NW)
genetic_screens_menu["menu"].config(bg="white", activebackground='#a9f5e8', activeforeground='black')

custom_1_str=tk.StringVar()
custom_1_list=sorted(retrieve_col(retrieve="written_name", filter="category", select=category_dict['custom_1']))
if not custom_1_list:
    custom_1_list.append("<EMPTY>")
custom_1_menu = tk.OptionMenu(root, custom_1_str, *custom_1_list, command=study_select)
custom_1_menu.config(indicatoron=0, height=12, relief='flat', background='white', font=("Calibri",11), highlightbackground='white', activebackground='white', compound=CENTER, image=drop_img)
custom_1_menu.grid(row=3, column=3, columnspan=2,  sticky=NW)
custom_1_menu["menu"].config(bg="white", activebackground='#a9f5e8', activeforeground='black')

custom_2_str=tk.StringVar()
custom_2_list=sorted(retrieve_col(retrieve="written_name", filter="category", select=category_dict['custom_2']))
if not custom_2_list:
    custom_2_list.append("<EMPTY>")
custom_2_menu = tk.OptionMenu(root, custom_2_str, *custom_2_list, command=study_select)
custom_2_menu.config(indicatoron=0, height=12, relief='flat', background='white', font=("Calibri",11), highlightbackground='white', activebackground='white', compound=CENTER, image=drop_img)
custom_2_menu.grid(row=3, column=5, columnspan=2,  sticky=NW)
custom_2_menu["menu"].config(bg="white", activebackground='#a9f5e8', activeforeground='black')

study_select("<EMPTY>")




# Dataset text box
output_text2_frame = ttk.Frame(style="RoundedFrame", padding=10)
output_text2 = tk.Text(output_text2_frame, borderwidth=0, highlightthickness=0, wrap="word", height=10, width=45, font=("Calibri",12))
output_text2.grid(row=0, rowspan=2, column=0, columnspan=2, sticky=NSEW)
Grid.columnconfigure(output_text2_frame, 0, weight=1)
Grid.rowconfigure(output_text2_frame, 0, weight=1)
output_text2.bind("<FocusIn>", lambda event: output_text2_frame.state(["focus"]))
output_text2.bind("<FocusOut>", lambda event: output_text2_frame.state(["!focus"]))
output_text2_frame.grid(row=5, column=3, columnspan=4, sticky=N+S+E+W)
Grid.columnconfigure(root, 3, weight=1)
# Top hits button
tophits_frame = ttk.Frame(master=root, style="RoundedFrame", padding=10)
topsearch = tk.Button(tophits_frame, text="Top Hits", font=("Calibri bold",12), background='white', relief='flat', command=lambda: top_search(omic_string=output_text2.get("1.0",END)))
topsearch.pack(fill="both", expand=True)
topsearch.bind("<Button-1>", lambda event: tophits_frame.state(["focus"]))
topsearch.bind("<ButtonRelease-1>", lambda event: tophits_frame.state(["!focus"]))
topsearch.config(activebackground='white')
tophits_frame.grid(row=4, column=3, stick=W)
# Overlap button
compare_frame = ttk.Frame(master=root, style="RoundedFrame", padding=10)
compare = tk.Button(compare_frame, text="Compare", font=("Calibri bold",12), background='white', relief='flat', command=lambda: det_overlap(liststr=output_text2.get("1.0",END)))
compare.pack(fill="both", expand=True)
compare.bind("<Button-1>", lambda event: compare_frame.state(["focus"]))
compare.bind("<ButtonRelease-1>", lambda event: compare_frame.state(["!focus"]))
compare.config(activebackground='white')
compare_frame.grid(row=4, column=4, stick=E)
# Clear button
clear_frame = ttk.Frame(master=output_text2_frame, style="RoundedFrame", padding=10)
clear_button = tk.Button(clear_frame, text="Clear", width=7, relief='flat', font=("Calibri",10), background='white', command=lambda: output_text2.delete("1.0", END))
clear_button.pack()
clear_button.bind("<Button-1>", lambda event: clear_frame.state(["focus"]))
clear_button.bind("<ButtonRelease-1>", lambda event: clear_frame.state(["!focus"]))
clear_button.config(activebackground='white')
clear_frame.grid(row=1, column=1, sticky=SE)










topsep = ttk.Separator(root, orient=HORIZONTAL)
topsep.grid(row=1, column=0, sticky="new", columnspan=7)

root.after(0, fast_load)
root.mainloop()


