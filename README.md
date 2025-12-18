# repboxAI

Does all sorts of information extraction from scientific articles and reproduction packages using AI an heuristics. Main aspect is to deal with fact that there are many ways to extract information (prompts / models) and many are often imperfect. 

## Concepts

### product 

A product is an extracted data set with well specified columns that strictly follow a provided schema. The different products are defined in `repbox_products()`. 

Here a small subset:

```r
prods = prods_define(
  prod_define("tab_list",
    descr = "List of article's tables",
    list(
      tabid = schema_str(maxLength = 10),
      otabid = schema_str() # ordered tabid by augmenting numbers with 0s from left
    )
  ),
  prod_define("tab_tino",
    descr = "List of article's tables with extracted title and table notes",
    widens = "tab_list",
    list(
      tabtitle = schema_str(maxLength=400),
      tabnotes = schema_str(maxLength=2000)
    )
  ),
  prod_define("tab_html",
    descr = "Contains normalized HTML of every extracted article table",
    widens = "tab_tino",
    list(
      tabhtml = schema_html_tab()
    )
  ),
  prod_define(
    "cell_list",
    # means 1 parent row can have multiple children rows
    parent = "tab_html",
    from_parent = c("tabid","otabid"),
    fields = list(
      cellid = schema_str(),
      row = schema_int(),
      col = schema_int(),
      content = schema_str(),
      colspan = schema_int(),
      rowspan = schema_int()
    )
  ),
  prod_define(
    "cell_base",
    widens = "cell_list",
    fields = list(
      is_num = schema_bool("is_num"),
      has_deci = schema_bool("has_deci"),
      braces = schema_str(enum=c("", "(","[","{")),
      has_sig_star = schema_bool(),
      sig_star = schema_str()
    ),
    descr ="Can be generated purely using heuristics from td_list"
  )
)
```

### version

A version describes a particular process to generate a product, e.g. defined by the AI model and exact prompt. Some versions may use only heuristics, others only AI, others a mix. There will be typically several versions of a product and it is hard to say ex-ante which is the best: we need to create, test, rank and select versions. `repboxAI` helps to deal with multiple versions.

### production run / product instance

Manufacturing the same product version multiple times, may not always yield exactly the same data set. AI results are typically stochastic, thus running the same process to create a version may yield different product instances. 

Results of production run will be stored in the following `run_dir`:

`{project_dir}/rai/prod_runs/{pid}/{vid}/r{run_ind}`

where `pid` is a product id, `vid` is the version id and, `run_ind` is the number of the production run for that particular product version. 

If a production run finished without error then `run_dir` will contain at least:

- `prod_df.Rds` the generated data frame
- `version.Rds` information about the version

If there was an error it will contain at least the file

- `has_error.txt`

### input products

Creating a product often uses a particular instance of another product as input. We try to store info on the production chain using the files `input_info.Rds` that store infos on the directly used input product instances in each `run_dir`.

### product tests

An important and complex issue will be to test product instances and evaluate them. One can think of different types of tests.

#### Single instance tests 

Have tests that you apply on a single instance. E.g. for `tab_list` check whether `tabid` follows an increasing sequence of table number 1,2,3, A1,A2, ... without holes. Such a particular test can also be used for derrived products like `tab_tino` and `tab_html`. 

#### Multi instance tests 

Compare different product instances. E.g. do two `tab_list` instances have the same vector of `tabid`

#### Ai supported tests

We will have pure heuristic tests, but perhaps also use AI for tests. E.g. decide which extracted table html will be the better on.

## The version network

One important task is to decide on the appropriate set of versions we consider when creating products. One can think of many prompt an model variations. 

If you have two input goods which also have a lot of versions, we also must decide which versions to select for our version. (Even more, we need a rule to select a particular instances). 

As each AI call takes time (and possibly money), we need to think hard on limiting the set of explored product versions. Also version naming is an important issue. Each combination of product id and version id should be unique and not super long!


### DDP: directly derived product instances

A `cell_list` instance will be deterministically computed from a `tab_html` instance using our function `normalized_html_tab_to_cell_df`. No AI or any randomness necessary. We have a convention for such *directly derived product instances*:

Each instance of the original product has a corresponding instance of the derived product with the same version id `vid` and `run_ind`. Only the product id `pid` is changed.

For example if an original `tab_html` instance has `run_dir`:

`{project_dir}/rai/prod_runs/tab_html/tab_html_hx_pdf/r5`

then its directly derived product instance of `cell_list` has `run_dir`:

`{project_dir}/rai/prod_runs/cell_list/tab_html_hx_pdf/r5`

`repboxAI` has several helper functions with prefix `ddp_` that help to quickly generate directly derived product instances.

A product may in principle hve both: versions only with directly instances from a parent product or also versions generated by some alternative process.

### backported product instances

The product `tab_tino` is a wider version of `tab_list` with the additionl columns `tabtitle` and `tabnotes`. One could first produce `tab_list` and then add with subsequent AI calls or heuristics the `tabtitle` and `tabnotes`. 

Alternatively, our AI prompt may directly generate a `tab_tino` instance. By removing the columns `tabtitle` and `tabnotes` we can generate a backported `tab_list` instance of this `tab_tino` instance. This might be useful, as our tests for `tab_list` may compare backported versions with other versions. We can then better understand how fine-grained our extraction steps should be.

Naming conventions for backported versions and instances are similar as for directly derived products:

For example, if the `tab_tino` instance has `run_dir`:

`{project_dir}/rai/prod_runs/tab_tino/tab_tino-j-g2f-0-pdf-1/r2`

then the backported `tab_list` instance has `run_dir`:

`{project_dir}/rai/prod_runs/tab_list/tab_tino-j-g2f-0-pdf-1/r2`

# Function prefixed in repboxAI

#### raix_

Extract a product using AI. E.g. `raix_tab_html_pdf` creates `tab_html` instances using AI. The `_pdf` indicates that the AI extracts the information directly from the PDF of the article. 

#### rai

A `rai` (repbox AI) object stores informations related to a single AI call that will usually be part of an `raix_` extraction function.

There are a lot of `rai_` helper functions. E.g. `rai_context` generates a context object that can contain documents like the article PDF that can be cached using gemini's context caching function. The helper functions automate often repated stuff.

#### rais

Often a `raix_` extraction function makes multiple similar AI calls. For example, when creating a `tab_html` product. We ask the AI separately for each table identified in a  `tab_tino` input, to extract the HTML version and return it. 

A `rais` object stores the relevant information of such multiple AI calls in a nice way to conveniently generate product instances. Even if a `raix_` function uses a single AI call, we will typically generate a `rais` object, since we have created convenience functions like `rais_save` that saves all relevant files of an AI-generated product instance mainly for `rais` objects.


### ddp_

Helper functions for directly derived product instances (see further above). 

