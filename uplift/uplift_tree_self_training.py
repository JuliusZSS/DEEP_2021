# Input: synthetic data
# Output: uplift result

def uplift_tree_self_training(folder_name, batch_number, input_csv_name, image_name, max_depth):

    import pandas as pd
    from causalml.inference.tree import UpliftTreeClassifier
    from IPython.display import Image
    from causalml.inference.tree import uplift_tree_plot

    # read in data
    data = pd.read_csv(input_csv_name)

    # define treatment and outcome
    features = range(1, folder_name + 1)
    features_name = list(data.columns)[1:]
    treatment = 'w'
    outcome = 'y'

    # set up uplift model
    # change evaluationFunction from 'KL' to 'CTS' or 'ED'
    uplift_model = UpliftTreeClassifier(max_depth=max_depth, min_samples_leaf=200, min_samples_treatment=50,
                                        n_reg=100, evaluationFunction='ED', control_name='0')

    # train model
    uplift_model.fit(data.iloc[:, features].values,
                     treatment=(data[treatment].astype(str)).values,
                     y=data[outcome].values)

    # visualise model
    graph = uplift_tree_plot(uplift_model.fitted_uplift_tree, features_name)
    uplift_model_structure = Image(graph.create_png())
    uplift_model_png = image_name
    with open(uplift_model_png, 'wb') as o:
        o.write(uplift_model_structure.data)

    # get prediction using train dataset
    predictions = uplift_model.predict(data.iloc[:, features].values)

    return predictions[1]
