<div class="container-fluid main-container">


<div class="row">


<div class="toc-content col-xs-12 col-sm-8 col-md-9">


<div id="header">


# MacBehaviour: Conduct Psychological Experiments on LLMs

</div>
<img width="859" alt="image" src="https://github.com/user-attachments/assets/08b8327d-20d7-4a17-ab0a-15b5c65033b2">




<br><br>
The `MacBehaviour` R package offers a user-friendly toolkit for conducting  psychological experiments on over 100 Large langauge models(LLMs) in a few lines.

Please visit <a href="https://github.com/xufengduan/MacBehaviour">here</a> for the latest information.

For details and citation, please see the preprint: <a href="https://arxiv.org/abs/2405.07495"> Duan, X., Li, S., & Cai, Z. G. (2024). MacBehaviour: An R package for behavioural experimentation on large language models. </a>

<div id="installing-and-loading-necessary-packages" class="section level3">

## News

2024-Sep-5: Support logging Logprobs for Chat models on Hugging Face.<br>
2024-July-2: Support models on Qianfan Baidu (百度千帆大模型平台).

## Table of Contents
- [Supported Model Platforms](#supported-model-platforms)
- [Supported Models](#supported-models)
- ⭐️[Installation](#installation)
- ⭐️[Demo Code](#demo-code)
- [Tutorial](#tutorial)
  - [1. Communicate with Models](#1-communicate-with-models)
  - [2. Experiment Design](#2-experiment-design)
  - [3. Demo Experiments](#3-demo-experiments)
  - [4. Result Structure](#4-result-structure)


## Supported Model Platforms

This package enables local deployment of LLMs through **FastChat** (https://github.com/lm-sys/FastChat).

If you prefer using cloud-based models, this package currently supports the following platforms:

1. [OpenAI](https://platform.openai.com/)
2. [Hugging Face](https://huggingface.co/)
3. [Claude](https://www.anthropic.com/api)
4. [Gemini](https://ai.google.dev/)
5. [Qianfan Baidu](https://qianfan.cloud.baidu.com/)
6. [Baichuan](https://platform.baichuan-ai.com/)
7. [AI/ML](https://aimlapi.com/)

## Supported Models

| Model                                                     | Developer/Platform                |
| :-------------------------------------------------------- | --------------------------------- |
| GPT  family  (GPT-3.5,  GPT-4 et al.)                     | OpenAI (OpenAI et al., 2024)      |
| Claude family  (Haiku, Sonnet, Opu et al.)                | Anthropic (Anthropic,  2023)      |
| Gemini family  (Ultra, Pro, and Nano et al.)              | Google (Gemini Team et al., 2023) |
| Llama  family  (Llama-2,  Llama-3)                        | Meta  (Touvron et al., 2023)      |
| BaiChuan family  (7B, 13B et al)                          | Baichuan (Yang et al., 2023)      |
| 50+  other self-hosted LLMs  (e.g.,  Vicuna, FastChat-T5) | FastChat (Zheng  et al., 2023)    |
| 200+ other cloud-hosted LLMs	                            | AI/ML API (AI/ML API, 2024)       |


## Installation

There are two ways for installing this package: from Git hub or CRAN

```R
# From github
install.packages("devtools")
devtools::install_github("xufengduan/MacBehaviour")

```

<div id="set-api-key" class="section level3">
Or you can install the package from CRAN by

```R
# From CRAN
# install.packages("MacBehaviour")
```


Upon the successful installation, users can load this package into the current R session:

``` R
library("MacBehaviour")
```
## Demo Code
```R
# Communicate with LLMs (refer to the tutorial for instructions on obtaining the API key)
setKey(api_key = "YOUR_API_KEY", model = "MODEL_ID") 

# Load the stimuli (experimental design is based on specific columns in the stimuli file)
# Example data can be found in the Material folder (https://github.com/xufengduan/MacBehaviour/tree/main/Materials)
df = read.xlsx("Data_OTPR.xlsx")

# Standardize the data frame
ExperimentItem = loadData(runList = df$Run, itemList = df$Item, eventList = df$Event, conditionList = df$Condition, promptList = df$Prompt)

# Input experiment-specific parameters here
Design = experimentDesign(ExperimentItem, session = 1, randomItem = FALSE)

# Input model-specific parameters here
gptConfig = preCheck(systemPrompt = "You are a participant in a psychological experiment.", data = Design, n = 3, max_tokens = 2)

# Run the experiment!
runExperiment(gptConfig, savePath = "./demo.xlsx")
```

## Tutorial
### 1\. Communicate with Models

Authenticate with LLMs using an API key.

    setKey(api_key = "YOUR_API_KEY", model = "YOUR_MODEL")
    
    # you need to input an additional argument secrect_key = "YOUR_SCRECT_KEY" here to access to Baidu Qianfan platform.
    
    # Then you will receive a message:
    
    ## "Setup api_key successful!"

Arguments: Replace `YOUR_OPENAI_API_KEY` and  `YOUR_MODEL` with your personal key and selected model index.

1) The "api_key" argument, required, needs the user's personal API (Application Programming Interface) from OpenAI, Hugging Face, or other companies. If users are using a self-hosted model, please enter "NA." API enables authenticated access to language models. Researchers interested in obtaining OpenAI API key should first sign up on the OpenAI platform (https://platform.openai.com/). After registration, navigate to user’s account settings where user can generate personal API key. Similarly, for Hugging Face models, an API key specific to Hugging Face is required. This can be obtained by creating an account on the Hugging Face platform (https://huggingface.co/). Once you are logged in, access your account settings, and find the "access token" to generate Hugging Face API key. Please note that model inference requires computational resources, and users may need to pay for inference costs. You can find pricing details for OpenAI (https://openai.com/pricing) and Hugging Face (https://huggingface.co/blog/inference-pro). 

2) The "model" argument, required, a character vector, specifies the index of the selected model. For OpenAI models, you can find the list of available model indexes here: (https://platform.openai.com/account/limits). For Hugging Face models, the model name corresponds to the repository name (e.g., meta-llama/Llama-2-13b-hf). A list of available models can be found (https://huggingface.co/models?inference=warm&other=text-generation-inference&sort=trending). For self-hosted models, users can find the model's name at the model’s corresponding repository (for a summary, see https://github.com/lm-sys/FastChat/blob/main/docs/model_support.md).

3) The "api_url" argument, optional, a character vector, specifies the interface domain of the selected model. By default, the system will automatically determine the appropriate URL based on the user’s "api_key". Users can still specify a custom api_url, which will take precedence. For experiments using the GPT family, the URLs are documented in OpenAI's API reference (https://platform.openai.com/docs/api-reference/authentication). For Llama models available through Hugging Face, the model’s URL can be found in the respective model’s repository, such as " https://api-inference.huggingface.co/models/meta-llama/Llama-2-70b-chat-hf". For self-hosted models, please fill this argument with the user’s local URL ("for more information, see https://github.com/lm-sys/FastChat/blob/main/docs/openai_api.md).

</div>

<div id="experiment-design" class="section level3">


### 2\. Experiment Design:

"MacBehaviour" can implement an experiment in two types of designs. 

**1) multiple-trials-per-run design** resembles typical psychological experiments, where a human participant encounters multiple trials in an experiment. Here, you present multiple experimental trials, one by one, to an LLM in a single conversation. Note that earlier input and output will serve as the context for a current trial. 

<img width="442" alt="MTPR" src="https://github.com/user-attachments/assets/8a03e224-b753-43c8-abaa-9170299bc97f">


**2) one-trial-per-run design**, you only present a single trial of prompt and stimulus to an LLM in a conversation, and you present another trial in a new conversation.

<img width="419" alt="OTPR" src="https://github.com/user-attachments/assets/999fac7e-e093-495f-ab45-1bd1ed1d6a18">



### 3\. Demo Experiments:
To illustrate these designs and how to construct the experimental stimuli, we next use a demo experiment. 

Cassidy et al. (1999) showed that speakers of English can infer the gender of novel personal names from phonology. 

In particular, when asked to complete a sentence fragment:

 *After Corlak/Corla went to bed …*

People tend to use a masculine pronoun for names ending in a closed syllable (e.g., *Corlak*) but a feminine pronoun for those ending in an open syllable (e.g., *Corla*). 

In our demo, we ask an LLM to complete sentence fragments and observe how the model refers to the novel personal name (e.g., using masculine pronouns such as *he/him/his* or feminine ones such as *she/her/hers*).

1. **multiple-trials-per-run design**,
Before using this package, users should prepare one Excel/CSV file/data frame containing the experimental stimuli and other information for experiment design (see Table 1).



   **Table 1**. **The data frame structure**

   | **Column**    | **Description**                                              |
   | ------------- | ------------------------------------------------------------ |
   | **Run**       | The index of the conversation with the  model, akin to the concept of "list" in a psychological experiment.  Items shared with the same Run index will be presented in a single  conversation. |
   | **Item**      | Indicates the item index of stimuli for  data tracking and organization. |
   | **Condition** | Specifies the experimental condition  associated with each stimulus, for researcher's reference. |
   | **Prompt**    | Contains the actual prompt,  together with a stimulus, presented to the model |

   **Note.** Each row stands for a unique stimulus in the data frame/sheet.

The Excel file/data frame should exhibit a structured format, defining columns for "Run", "Item", "Condition", and "Prompt", with each row standing for a unique stimulus (see Table 1 for a description of these terms and Table 2 for an example). 

In the multiple-trials-per-run design, multiple trials (four trials in our demo) are presented in a single conversation (Run). 

In each Run, the package will send the stimulus based on the index of row. Users can randomize item order within Runs in the function "experimentDesign" later. The LLM will use input (prompts and stimuli) and model output (responses) in earlier trials as its context (see Figure 1 for an example of conversation/Run).

   **Table 2**. **An exemplar stimulus file in a multiple-trials-per-run design**

   | **Run** | **Item** | **Condition**    | **Prompt**                                                   |
   | ------- | -------- | ---------------- | ------------------------------------------------------------ |
   | 1       | 1        | Open  syllable   | Please repeat the fragment and complete it  into a full sentence: Although Pelcra was sick … |
   | 1       | 2        | Closed syllable  | Please repeat the fragment and complete it  into a full sentence: Because Steban was very careless … |
   | 1       | 3        | Open  syllable   | Please repeat the fragment and complete it  into a full sentence: When Hispa was going to work … |
   | 1       | 4        | Closed  syllable | Please repeat the fragment and complete it  into a full sentence: Before Bonteed went to college … |
   | 2       | 1        | Closed  syllable | Please repeat the fragment and complete it  into a full sentence: Although Pelcrad was sick … |
   | 2       | 2        | Open  syllable   | Please repeat the fragment and complete it  into a full sentence: Because Steba was very careless … |
   | 2       | 3        | Closed  syllable | Please repeat the fragment and complete it  into a full sentence: When Hispad was going to work … |
   | 2       | 4        | Open  syllable   | Please repeat the fragment and complete it  into a full sentence: Before Bontee went to college … |


   There are two roles in the context: "user" (for sending stimuli) and "assistant" (as a participant to provide responses). To achieve the above conversation, this package sends the stimuli in the following format for OpenAI GPT series/open-source models and Llama2:

    

   ```r
   # OpenAI/Open-source models
   
   # For the first trial: 
   
   list(role = "user", content = "Please repeat the fragment and complete it into a full sentence: Although Pelcra was sick … ") 
   
    
   
   # For the second trial:
   
   [list (role = "user", content = "Please repeat the fragment and complete it into a full sentence: Although Pelcra was sick … "),
   
   list (role = "assistant", content = " Although Pelcra was sick, she remained determined to finish her project on time. "),
   
   list (role = "user", content = " Please repeat the fragment and complete it into a full sentence: Because Steban was very careless …")]
   ```

    

   The conversational context was provided as the beginning of the next trial’s prompt. In this example, the context included the first stimulus *Please repeat the fragment and complete it into a full sentence: Although Pelcra was sick …* and its response *Although Pelcra was sick, she remained determined to finish her project on time.* The prompt then presented the second stimulus *Please repeat the fragment and complete it into a full sentence: Because Steban was very careless …* after the conversational context. We implemented this function for Llama-2-chat-hf series in the same way (see more at [https://Huggingface.co/blog/llama2#how-to-prompt-llama-2](https://huggingface.co/blog/llama2#how-to-prompt-llama-2)). 

    

2. **One-trial-per-run Design**

    In the one-trial-per-run design, an LLM will be presented only one trial of the experiment in a Run/conversation. In our demo experiment (see Table 3), for instance, each conversation with the LLM involves only one stimulus. In this design, each stimulus is given a unique Run number, indicating that each one is to be presented in a separate conversation with the LLM. This design eliminates the potential for previous context to influence the response of current stimulus, ensuring that each stimulus is evaluated independently.

    

   **Table 3**. **Stimuli for one-trial-per-run design**

   | **Run** | **Item** | **Condition**   | **Prompt**                                                   |
   | ------- | -------- | --------------- | ------------------------------------------------------------ |
   | 1       | 1        | Open syllable   | Please repeat the fragment and complete it  into a full sentence: Although Pelcra was sick … |
   | 2       | 1        | Closed syllable | Please repeat the fragment and complete it  into a full sentence: Although Pelcrad was sick … |
   | 3       | 2        | Open syllable   | Please repeat the fragment and complete it  into a full sentence: Because Steba was very careless … |
   | 4       | 2        | Closed syllable | Please repeat the fragment and complete it  into a full sentence: Because Steban was very careless … |
   | 5       | 3        | Open syllable   | Please repeat the fragment and complete it  into a full sentence: When Hispa was going to work … |
   | 6       | 3        | Closed syllable | Please repeat the fragment and complete it  into a full sentence: When Hispad was going to work … |
   | 7       | 4        | Open syllable   | Please repeat the fragment and complete it  into a full sentence: Before Bontee went to college … |
   | 8       | 4        | Closed syllable | Please repeat the fragment and complete it  into a full sentence: Before Bonteed went to college … |

    

Load your stimuli from an Excel file.

    df = read.xlsx("/path/to/excel/demo.xlsx")

The "read.xlsx" function from the "openxlsx" package reads the Excel file, converting it into a data frame within R. You can also import a data frame containing stimuli and experiment information through other functions. To accurately present the stimuli within the R environment, the "loadData" function is utilized, which organized the data from a data frame for further processing:

<table>
<thead>

<tr class="header">

<th>Run</th>

<th>Item</th>

<th>Condition</th>

<th>Content</th>

</tr>

</thead>

</table>

The `read.xlsx` function from the `openxlsx` package reads the file, converting it into a data frame within R. To accurately use the stimuli within the R environment, the `loadData` function is utilized, which translates the structured data from an Excel file/data frame into an organized data frame within R:

```r
ExperimentItem = loadData(runList=df$Run, itemList=df$Item, conditionList=df$Condition, promptList=df$Prompt)
```

Arguments: This function prepares the stimuli from your Excel data.

The "loadData" function maps vectors or data frame columns to specific keywords. These keywords are then recognized by subsequent functions in our framework. This mapping streamlines the automatic identification and processing of relevant data collection:

1) The "runList", required, a numeric vector, matches the column for "Run" in the CSV file and denotes the conversation/run index. It is utilized in loops for interactions with LLMs. The vector's name (e.g., df$Run) can be arbitrary; what's important is the content specified by users for the runList. This applies to subsequent parameters in this function as well.

2) The "itemList", required, a numeric vector, refers to the column for "Item", indicating the item index of stimuli. This is for the researcher's reference and does not interact with the model's operation. It will be used in loops for interactions with LLMs.

3) The "conditionList", required, a numeric/character vector, represents the column for "Condition", which specifies the experimental condition associated with each stimulus. Similar to "itemList", it is for the researcher's reference and does not interact with the model's operation.

4) The "promptList", required, a character vector, maps to the column for "Prompt", which contains the actual prompts that will be presented to the model during the experiment. Each element under this column is a unique prompt the language model will process and respond to. 


The output of this function, "ExperimentItem", is a data frame generated by "loadData", which includes all the necessary details for each stimulus. The accuracy of "loadData" in mapping the CSV spreadsheet/data frame to the "ExperimentItem" is of pivotal importance, as it ensures that each stimulus is precisely presented according to the experimental design.

   Next, the "experimentDesign" function allows users to define the structure and sequence of the experimental Runs (conversation):

   ```R
   Design = experimentDesign(ExperimentItem, session = 1, randomItem = F)
   ```

   1) "ExperimentItem", required, a data frame, is the output of function "loadData", which is a structured data frame for storing stimuli and experimental information (e.g., item, condition, and prompt for each stimulus).
   2) The "Session", optional, an integer, specifies the number of iterations for all stimuli. The default value is 1. It adds a new column named "session" to your data frame, where each session includes all original stimuli. If the "Session" is set to 2, the package collects data for one session and then repeats all stimuli for a second session.
   3) "randomItem", optional, a logical vector, is available to randomize the order of item presentation within a run (conversation). It automatically remains "FALSE" for the one-trial-per-run design. 

<div id="model-parameters" class="section level3">


3. **Model Parameters**

The model parameters are configured to guide the behaviour of the model during the experiment in the "preCheck" function:

 

```r
gptConfig = preCheck (data = Design, checkToken = F, systemPrompt = "You are a participant in a psychological experiment", max_tokens = 500, temperature = 0.7, n = 1)
```

 

1)"data", required, a data frame, is the output of experimentDesign function.

2) The "systemPrompt", optional, a character vector, offers a task instruction to the model analogous to the instructions given to participants in a psychological experiment. Should one wish to convey the instructions to the model through the trial prompt, one could leave this parameter blank or state some general instructions (e.g., "You are a participant in a psychological experiment, please follow the task instruction carefully"). By default, it is empty. If not, the package will send the systemPrompt content at the start of each run.

``` r
[list(role = "system", content = " You are a participant in a psychological experiment, please follow the task instruction carefully."),
(role = "user", content = "Please repeat the fragment and complete it into a full sentence: Although Pelcra was sick …"),
…]
```

3) The "max_tokens", optional, a numeric vector, limits the length of the model's response. This may lead to an incomplete response if the tokens of response intended by a model exceed this value. The default is Null.

4) The "checkToken", optional, a logical vector, allows users to conduct a token count in order to determine whether their trial(s) have more tokens than a model allows in a single conversation. The default setting, however, is FALSE. When set to TRUE, the package initiates the upload of your experimental stimuli to the tokenizer server of this package for token counting (note that your stimuli will not be retained on the server; they will be promptly removed after the necessary calculations are completed). Our server uses tokenizer algorithms from OpenAI (https://github.com/openai/tiktoken) and Hugging Face (https://github.com/huggingface/transformers/), supporting over 250 models, including OpenAI family, Llama and BERT, automatically selecting the appropriate tokenizer for each. If an unsupported model is chosen, users are alerted with a warning in their report indicating that results were calculated using GPT-2 as the default tokenizer. This ensures transparency about which tokenizer was used, helping users make informed decisions.
   For example, consider a study with a one-trial-per-run design that includes 40 items and 100 sessions, where the item with the highest number of tokens has 137. The "checkToken" function generates tailored reports according to your experiment's design. For instance:

    ```r
    # One-trial-per-run design
    #	CheckItem			Values
    # 1	item numbers		   4000
    # 2	max_token_numbers	    137
    ```
    In the report, the "item numbers" show the number of items you have (number of items × number of sessions). The value of "max_token_numbers" signifies the maximum token length among all experimental items. It should not exceed the input token limit of an LLM. 

    ```r
    # Multiple-trials-per-run design
    # Run		max_tokens_per_run
    # 1		    1756
    # 2 		2016
    # …
    ```
    
    In the report for multiple-trials-per-run design, the package computes the input for the last trial of a run—incorporating all previous conversation history—based on the maximum token count. This is calculated as (systemPrompt + max_tokens) × number of trials + previous conversation history + tokens from the last item; it then reports this total for each run. Please make sure that the max token per run does not exceed the token limit of your selected LLM. The following is an example report. 

5) The "logprobs", optional, a boolean vector, specifies whether to return the log probabilities of output tokens in the chat completion mode. It appends the log probability for each token in the response under the "rawResponse" column. Additionally, users can define how many top probable tokens to display at each token position by introducing a numeric vector “top_logprobs” (https://platform.openai.com/docs/api-reference/chat/create#chat-create-logprobs), which ranges from 0 to 20, showing their corresponding log probabilities. Please note that "logprobs" must be active for this feature to work. Setting it to 2 returns the two most likely tokens at that position. For instance, if "logprobs" is set to TRUE and "top_logprobs" is set to 2, a generated response might be: “Hello! How can I assist you today?” For the first token “Hello”, two alternatives are provided:

``` {"top_logprobs": [{"token": "Hello", "logprob": -0.31725305}, {"token": "Hi", "logprob": -1.3190403}]}```

- This configuration also provides the two most probable tokens and their respective log probabilities for each subsequent token position. 
In the text completion mode (detailed in section "api_url" part in session 2 .1) in the GPT family, "logprobs" is limited to a numeric vector with a maximum value of 5; hence, users don’t need to specify candidates by "top_logprobs" (https://platform.openai.com/docs/api-reference/completions/create#completions-create-logprobs). For self-hosted models, currently, only text completion supports collecting token probabilities by setting logprobs to True. This randomly returns one token and its probability at a time, but users can continue requesting until they receive the desired token.

6) imgDetail, optional, offers three settings for image input: low, high, or auto. This allows users to control the model's image processing and textual interpretation. By default, the model operates in "auto" mode, automatically selecting between low and high settings based on the input image size (see more for https://platform.openai.com/docs/guides/vision/low-or-high-fidelity-image-understanding). If inputs do not include images, please skip this parameter.

7) The "temperature", optional, a numeric vector, controls the creativity in LLM’s responses (https://platform.openai.com/docs/api-reference/chat/create#chat-create-temperature). 

8) The "n", optional, a numeric vector, determines how many unique and independent responses are produced by the model for a single trial. For example, if n = 20, users will get 20 unique responses for each request. However, in a multiple-trials-per-run design, this parameter is automatically disabled to prevent branching conversations (https://platform.openai.com/docs/api-reference/chat/create#chat-create-n). 
In addition to the parameters mentioned above, users can also enter optional ones. For reference, you can consult OpenAI's documentation (https://platform.openai.com/docs/api-reference/chat/create) or that of the selected model.

<div id="run-the-experiment" class="section level3">

4. **Run the Experiment**

1. The "runExperiment" function is the execution phase of data collection. It initiates the interaction with an LLM based on the specified design and parameters, and iteratively collects responses to the stimuli.

   

  ```r
runExperiment (gptConfig, savePath = "./demo.xlsx")
  ```

    1) "gptConfig" is the configuration list object containing all the details of the experiment setup, including the system prompt, chosen model, maximum tokens, temperature, the number of responses and other parameters. This object is crafted in the preceding steps "preCheck".
    2) "savePath" is the file path where the experiment results will be saved. This should be an accessible directory on the user's machine with the appropriate write permissions. A file name in the path with either the ".xlsx" or ".csv" extension indicates that its contents are saved in "xlsx" or "csv" format, respectively. These formats are particularly friendly for users who may wish to perform additional data manipulation or visualization within spreadsheet software or import the data into statistical software packages for further analysis.

  When "runExperiment" is active, the package sends a prompt to the selected language model, records the model’s output, and then moves on to the next stimulus as per the experiment design. 

<div id="run-the-experiment" class="section level3">



### 4\. Result Structure

Upon the completion of the experiment, the responses are compiled into a file. This file consists of multiple columns including Run, ItemID, Condition, Prompt, the corresponding response from the LLM and other information. The output file typically has the following columns:

 

 

**Table 6**. The data structure of output file.

| **Column**    | **Description**                                            |
| ------------- | ---------------------------------------------------------- |
| **Run**       | The conversation index.                                    |
| **Item**      | Indicates the Item number.                                 |
| **Condition** | Details the condition under which the item  was presented  |
| **Prompt**    | Contains the original stimulus content sent  to the model. |
| **Response**  | The model's response to the stimulus.                      |
| **n**         | The response index in a single request.                    |
| **Trial**     | The turn index of a conversation.                          |
| **Message**   | The actual prompt sending to an LLM.                       |
| **RawResponse**| The raw response from Model's API.                       |

</div>

### 
