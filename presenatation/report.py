import streamlit as st
import pandas as pd
import numpy as np
import matplotlib as plt
import plotly.express as px
import altair as alt
import streamlit as st
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats
from PIL import Image
import os


#st.title("Perceptitions and Insights toward AI ")

education = st.sidebar.selectbox('AI experience', ['Option 1','Knowledge of AI', 'Experience with AI'])
space = st.sidebar.selectbox('Space Optimization', ['Option 1','Knowledge of AI', 'Experience with AI'])
maintenance = st.sidebar.selectbox('Predictive Maintanence', ['Option 1','Knowledge of AI', 'Experience with AI'])
security = st.sidebar.selectbox('Security and Privacy', ['Option 1','Option 2','Option 2'])
impact= st.sidebar.selectbox('AI Impact', ['Option 1','Knowledge of AI', 'Experience with AI'])
# Display the selected options

# SIMONA
participants_distribution = Image.open('presenatation/Images/Simona/participants.png')
#demo_INT_INCR = Image.open("presenatation/Images/Simona/acc_3.png")
#demo_USED_AI = Image.open("presenatation/Images/Simona/used_ai.png")
#demo_KNOWLEDGE = Image.open("presenatation/Images/Simona/.png")

st.image(participants_distribution, width=800)





