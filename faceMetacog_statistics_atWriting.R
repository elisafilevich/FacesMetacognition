#The first thing I want to show is that the raters rated something meaningful. There are two ways to show this:
#1. ICC, or some kind of inter-rater agreement: That doesn't really work yet-
#2. correlation (or lme explanation) between the objective measure and the external rating

#here goes the attempt to get 2.
#External ratings are based on the face contents, not outline
#rater 2 (named 1 in the paper)
HumanRater2_2DprocOutlineNoOutline.model = lmer(distance_rater2_alltheface_transformed ~ distance_2Dproc_faceOutline + distance_2Dproc_faceNoOutline  + (1 |subjectNum) + (1 |exprIndex), facesData_centered, REML=FALSE)
summary(HumanRater2_2DprocOutlineNoOutline.model)

HumanRater2_2DprocOutlineNoOutline.null1 = lmer(distance_rater2_alltheface_transformed ~ distance_2Dproc_faceNoOutline  + (1 |subjectNum) + (1 |exprIndex), facesData_centered, REML=FALSE)

HumanRater2_2DprocOutlineNoOutline.null2 = lmer(distance_rater2_alltheface_transformed ~ distance_2Dproc_faceOutline + (1 |subjectNum) + (1 |exprIndex), facesData_centered, REML=FALSE)

anova(HumanRater2_2DprocOutlineNoOutline.model, HumanRater2_2DprocOutlineNoOutline.null1) #compares the added value of adding the outline allone
anova(HumanRater2_2DprocOutlineNoOutline.model, HumanRater2_2DprocOutlineNoOutline.null2)


#rater 3 (named as 2 in the paper)
HumanRater3_2DprocOutlineNoOutline.model = lmer(distance_rater3_alltheface_transformed ~ distance_2Dproc_faceOutline + distance_2Dproc_faceNoOutline  + (1 |subjectNum) + (1 |exprIndex), facesData_centered, REML=FALSE)
summary(HumanRater3_2DprocOutlineNoOutline.model)

HumanRater3_2DprocOutlineNoOutline.null1 = lmer(distance_rater3_alltheface_transformed ~ distance_2Dproc_faceNoOutline  + (1 |subjectNum) + (1 |exprIndex), facesData, REML=FALSE)
summary(HumanRater3_2DprocOutlineNoOutline)

HumanRater3_2DprocOutlineNoOutline.null2 = lmer(distance_rater3_alltheface_transformed ~ distance_2Dproc_faceOutline  + (1 |subjectNum) + (1 |exprIndex), facesData, REML=FALSE)
summary(HumanRater3_2DprocOutlineNoOutline)

anova(HumanRater3_2DprocOutlineNoOutline.null1, HumanRater3_2DprocOutlineNoOutline.model)
anova(HumanRater3_2DprocOutlineNoOutline.null2, HumanRater3_2DprocOutlineNoOutline.model)



#ok, now the critical comparison. This doesn't quite work with lmes. 
#But I think that the models here are conceptually wrong. Because there cannot be a (physical) interaction between the raters and the subjective judgements. And somehow lmers will always  
subjectiveOrExternal_rate2_2D.model.interaction = lmer(distance_2Dproc_faceNoOutline ~ distance_rater2_alltheface_transformed * judge_transformed_16 + (1 |subjectNum) + (1 |exprIndex), facesData_centered, REML=FALSE)
summary(subjectiveOrExternal_rate2_2D.model.interaction)

subjectiveOrExternal_rate2_2D.model = lmer(distance_2Dproc_faceNoOutline ~ distance_rater2_alltheface_transformed + judge_transformed_16 + (1 |subjectNum) + (1 |exprIndex), facesData_centered, REML=FALSE)
summary(subjectiveOrExternal_rate2_2D.model)

subjectiveOrExternal_rate2_2D.null = lmer(distance_2Dproc_faceNoOutline ~ distance_rater2_alltheface_transformed + (1 |subjectNum) + (1 |exprIndex), facesData_centered, REML=FALSE)

anova(subjectiveOrExternal_rate2_2D.null, subjectiveOrExternal_rate2_2D.model)

subjectiveOrExternal_rate3_2D.model = lmer(distance_2Dproc_faceNoOutline ~ distance_rater3_alltheface_transformed + judge_transformed_16 + (1 |subjectNum) + (1 |exprIndex), facesData_centered, REML=FALSE)
summary(subjectiveOrExternal_rate3_2D.model)

subjectiveOrExternal_rate3_2D.null = lmer(distance_2Dproc_faceNoOutline ~ distance_rater3_alltheface_transformed + (1 |subjectNum) + (1 |exprIndex), facesData_centered, REML=FALSE)

anova(subjectiveOrExternal_rate3_2D.null, subjectiveOrExternal_rate3_2D.model)

anova(subjectiveOrExternal_rate2_2D.model.interaction, subjectiveOrExternal_rate2_2D.model)

