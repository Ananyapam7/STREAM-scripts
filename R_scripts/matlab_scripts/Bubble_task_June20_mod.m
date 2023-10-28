clear all , close all, clc, close all hidden
% Define a starting folder.
topLevelFolder = "C:\idddp\START_ML\START_india_data_folders";
fig_saving_folder = 'C:\idddp\START_ML\Old_start\START_India_data_analysis_sharing\Bubble_popping_task\figures';
cd(topLevelFolder)

%filelist=dir('*.zip');
filelist = dir(topLevelFolder);
filelist(1:2) = [];
% Get list of all subfolders.
%allSubFolders = genpath(topLevelFolder);
% Parse into a cell array.
listOfFolderNames = {filelist.name};

%remain = allSubFolders;
%listOfFolderNames = {};
%{
while true
    [singleSubFolder, remain] = strtok(remain, ';');
    if isempty(singleSubFolder)
        break;
    end
    listOfFolderNames = [listOfFolderNames singleSubFolder];
end

%}
%% find group infor

clin_data = xlsread('C:\idddp\START_ML\Old_start\Clinical_data_new1.xlsx');

%% list of bad data from the observation at the time of testing
bad_data = [491,492,563]';

%%data format
bubble_row = {'BUBBLE_1','BUBBLE_2','BUBBLE_3','BUBBLE_4','BUBBLE_5','BUBBLE_6'};
bubble_colX = [1,5,9,13,17,21]; %% numbers does not count column1 which is text
bubble_colY = [3,7,11,15,19,23];
interrupted = [];
numberOfFolders = length(listOfFolderNames);
for kk = 1 : numberOfFolders
    % Get this folder and print it out.
    thisFolder = listOfFolderNames{kk};
    thisFolderPath = fullfile(topLevelFolder,thisFolder);
    
    %% now do my bit
    cd(thisFolderPath)
    % get colouring data file
    %filePattern1 = sprintf('%s/*.xlsx', thisFolder);
    xlFileNames = dir(thisFolderPath);
    xlFileNames(1:2) = [];
    xlFileNames={xlFileNames.name};
    ix=regexp(xlFileNames,'bubbles');
    ix=~cellfun('isempty',ix);
    
    if sum(ix)>0
        
        xlFileNames=xlFileNames(ix);
        xlFileNames = xlFileNames';
        
        clear filename1 txt
        distanceX2 = [];
        distanceY = [];
        counter1 = 1;
        filename1 = char(xlFileNames)
        
        %% find the last attempt made by the child
        [~, sheets] = xlsfinfo(filename1);
        st =char(sheets(1,end));
        [numbers, txt, everything] = xlsread(filename1,st); %% read xsl file
        
        id = thisFolder; %get backend ID
        expression = 'child_(\d+)_';
        matches = regexp(id, expression, 'tokens');

        childID = str2double(matches{1}{1});
        ppt(kk,1) = childID;
        
        %% match the clinical data
        for pp = 1:length(clin_data)
            if ppt(kk,1)  == clin_data(pp,1)
                clinical_data(kk,1:25) = clin_data(pp,1:25);%% 3=diagnosis, 4=vsms, 15=DQ, 19=cog_age, 22=INDT, 23=age, 24=gender, 25=diagnosis with ID_ASD as 4
                break
            else
                clinical_data(kk,1:25) = nan(1,25);
            end
        end
        %% is it a bad data
        for pp = 1:length(bad_data)
            if ppt(kk,1)  == bad_data(pp,1)
                invalid_data(kk,1) = 1;
                break
            else
                invalid_data(kk,1) = 0;
            end
        end
        
        %% adjust data rows
        add_n = zeros(10,34);
        numbers = [add_n;numbers];
        numbers(1:10,:) =nan;
        
        %% find out if the task was aborted
        interrupted(kk,1) = numbers(15,1);
        bubble_popped(kk,1) = numbers(11,1);
        
        %% get bubble task data
        if numbers(11,1)> 0
            force = numbers(:,29);
            mean_force(kk,1) = mean(force,'omitmissing');
            
            %% get the distance data
            for pp = 1:length(txt)
                for gg = 1:length(bubble_row)
                    rmatch = strcmp(txt(pp,1),bubble_row(1,gg));
                    if rmatch == 1
                        fprintf("this is pp %d \n",pp);
                        fprintf("this is gg %d \n",gg);
                        x_col = bubble_colX(1,gg);
                        y_col = bubble_colY(1,gg);
                        fprintf("this is xcol %d \n",x_col);
                        fprintf("this is ycol %d \n",y_col);
                        distanceX(counter1,1) = numbers(pp,x_col) - numbers(pp,31) ;
                        distanceY(counter1,1) = numbers(pp,y_col) - numbers(pp,33);
                        counter1 = counter1+1;
                    end
                end
            end
            
            %%remove signs
            distanceX1 = distanceX.^2;
            distanceY1 = distanceY.^2;
            distanceX2 = sqrt(distanceX1);
            distanceY2 = sqrt(distanceY1);
            
            %%now get the mean distances
            mean_disX(kk,1) = mean(distanceX2);
            mean_disY(kk,1) = mean(distanceY2);
            
            %% get the duration data
            [ms, m, d, h, mn, s] = datevec(txt{14,2}); %% for some reasone my m and d are recorded as 1 default
            [ms1, m1, d1, h1, mn1, s1] = datevec(txt{18,2});%% for some reasone my m and d are recorded as 1 default
            endtm = (mn*60000)+(s *1000)+ms;
            starttm = (mn1*60000)+(s1 *1000)+ms1;
            time_taken(kk,1) = endtm - starttm;
            
            
        elseif numbers (11,1)== 0
            mean_force(kk,1) = nan;
            mean_disX(kk,1) = nan;
            mean_disY(kk,1) = nan;
            time_taken(kk,1)= nan;
        end
    else
        mean_force(kk,1) = nan;
        mean_disX(kk,1) = nan;
        mean_disY(kk,1) = nan;
        time_taken(kk,1)= nan;
    end
end


%% put the data together
cd("C:\idddp\START_ML\Old_start\START_India_data_analysis_sharing\Bubble_popping_task\output")

All_bubble_data = [ppt, mean_force, mean_disX, mean_disY, interrupted, bubble_popped, clinical_data, invalid_data];
histcounts(All_bubble_data(:,9))
All_bubble_data(any(All_bubble_data(:,1)==0,2), : ) = [];  %remove rows with no ID
All_bubble_data(any(isnan(All_bubble_data(:,1)),2), : ) = [];  %remove rows with no ID
histcounts(All_bubble_data(:,9))
All_bubble_data(any(All_bubble_data(:,9)==0,2), : ) = [];  %remove rows with zero diagnosis
All_bubble_data(any(isnan(All_bubble_data(:,9)), 2), :) = [];%remove data with no diagnosis
histcounts(All_bubble_data(:,9))

disp("Done successfully");

%% remove data with no touch by child
All_bubble_data(any(isnan(All_bubble_data(:,2)), 2), :) = [];%remove data with no touch
histcounts(All_bubble_data(:,9))
All_bubble_data(any(All_bubble_data(:,32)==1,2), : ) = [];  %remove bad data
histcounts(All_bubble_data(:,9))

dlmwrite('bubble_data.txt',All_bubble_data,'\t')
xlswrite('bubble_data.xlsx',All_bubble_data)
csvwrite('bubble_data.txt',All_bubble_data)

%% group stats
cd('C:\idddp\START_ML\Old_start\START_India_data_analysis_sharing\Bubble_popping_task\Data_extraction');

contT = 1;
contA = 1;
contI = 1;
for RR = 1: length(All_bubble_data(:,1))
    if All_bubble_data(RR,9)==1
        bubF_TD (contT,1) = All_bubble_data (RR, 2);
        bubDX_TD (contT,1) = All_bubble_data (RR, 3);
        bubDY_TD (contT,1) = All_bubble_data (RR, 4);
        contT = contT+1;
    elseif All_bubble_data(RR,9)==2
        bubF_ASD (contA,1) =  All_bubble_data (RR, 2);
        bubDX_ASD (contA,1) =  All_bubble_data (RR, 3);
        bubDY_ASD (contA,1) =  All_bubble_data (RR, 4);
        contA = contA+1;
    elseif All_bubble_data(RR,9)==3
        bubF_ID (contI,1) =  All_bubble_data (RR, 2);
        bubDX_ID (contI,1) =  All_bubble_data (RR, 3);
        bubDY_ID (contI,1) =  All_bubble_data (RR, 4);
        contI = contI+1;
    end
end

MbubF_TD = mean(bubF_TD,'omitmissing');
SEbubF_TD = stderr(bubF_TD);
MbubF_ASD = mean(bubF_ASD,'omitmissing');
SEbubF_ASD = stderr(bubF_ASD);
MbubF_ID = mean(bubF_ID,'omitmissing');
SEbubF_ID = stderr(bubF_ID);

bubble_force  = [MbubF_TD, MbubF_ASD, MbubF_ID];
errorsF = [SEbubF_TD, SEbubF_ASD, SEbubF_ID];
datF = {bubF_TD, bubF_ASD, bubF_ID};

figure(4)
bar(bubble_force)
hold on
h = plotSpread(datF);
hold on
errorbar(bubble_force, errorsF,'k','LineStyle', 'none','LineWidth',2)
ylabel('Mean force used','FontSize',16,'FontWeight', 'bold','LineWidth',2)
xlabel('Groups ','FontSize',16,'FontWeight', 'bold')
set(gca,'xtick',1:3,'FontSize',12, 'FontWeight', 'bold','LineWidth',2)
set(gca,'xticklabels',({'TD','ASD', 'ID'}))
title('Bubble task: Force','FontSize',18,'FontWeight', 'bold')
set(gca,'box','off');
set(gcf,'Color',[1,1,1]);
set(h{1},'MarkerFaceColor',[0.85,0.33,0.10])
saveas(gcf,fullfile(fig_saving_folder,'force_groups'),'png')

%% figure 2

MbubDX_TD = mean(bubDX_TD,'omitmissing');
SEbubDX_TD = stderr(bubDX_TD);
MbubDX_ASD = mean(bubDX_ASD,'omitmissing');
SEbubDX_ASD = stderr(bubDX_ASD);
MbubDX_ID = mean(bubDX_ID,'omitmissing');
SEbubDX_ID = stderr(bubDX_ID);

Dis_X  = [MbubDX_TD, MbubDX_ASD, MbubDX_ID];
errorsDX = [SEbubDX_TD, SEbubDX_ASD, SEbubDX_ID];
datDX = {bubDX_TD, bubDX_ASD, bubDX_ID};

figure(5)
bar(Dis_X)
hold on
h = plotSpread(datDX);
hold on
errorbar(Dis_X, errorsDX,'k','LineStyle', 'none','LineWidth',2)
ylabel('Mean distance from center X axis','FontSize',16,'FontWeight', 'bold','LineWidth',2)
xlabel('Groups ','FontSize',16,'FontWeight', 'bold')
set(gca,'xtick',1:3,'FontSize',12, 'FontWeight', 'bold','LineWidth',2)
set(gca,'xticklabels',({'TD','ASD', 'ID'}))
title('Bubble task: Distance X axis','FontSize',18,'FontWeight', 'bold')
set(gca,'box','off');
set(gcf,'Color',[1,1,1]);
set(h{1},'MarkerFaceColor',[0.85,0.33,0.10])
saveas(gcf,fullfile(fig_saving_folder,'disX_groups'),'png')

%% figure 3

MbubDY_TD = mean(bubDY_TD,'omitmissing');
SEbubDY_TD = stderr(bubDY_TD);
MbubDY_ASD = mean(bubDY_ASD,'omitmissing');
SEbubDY_ASD = stderr(bubDY_ASD);
MbubDY_ID = mean(bubDY_ID,'omitmissing');
SEbubDY_ID = stderr(bubDY_ID);

Dis_Y  = [MbubDY_TD, MbubDY_ASD, MbubDY_ID];
errorsDY = [SEbubDY_TD, SEbubDY_ASD, SEbubDY_ID];
datDY = {bubDY_TD, bubDY_ASD, bubDY_ID};

figure(6)
bar(Dis_Y)
hold on
h = plotSpread(datDY);
hold on
errorbar(Dis_Y, errorsDY,'k','LineStyle', 'none','LineWidth',2)
ylabel('Mean distance from center Y axis','FontSize',16,'FontWeight', 'bold','LineWidth',2)
xlabel('Groups ','FontSize',16,'FontWeight', 'bold')
set(gca,'xtick',1:3,'FontSize',12, 'FontWeight', 'bold','LineWidth',2)
set(gca,'xticklabels',({'TD','ASD', 'ID'}))
title('Bubble task: Distance Y axis','FontSize',18,'FontWeight', 'bold')
set(gca,'box','off');
set(gcf,'Color',[1,1,1]);
set(h{1},'MarkerFaceColor',[0.85,0.33,0.10])
saveas(gcf,fullfile(fig_saving_folder,'disY_groups'),'png')

[p,tbl,stats] = anova1(All_bubble_data(:,2),All_bubble_data(:,9));
[p,tbl,stats] = anova1(All_bubble_data(:,3),All_bubble_data(:,9));
[p,tbl,stats] = anova1(All_bubble_data(:,4),All_bubble_data(:,9));

disp("Program Complete");

% corr 

%force
force = All_bubble_data (:, 2);
indt = All_bubble_data (:, 28);
[rho_f,pval_f] = corr(force,indt,'Type','Spearman','Rows','pairwise');
fprintf("Rho value: %d \n",round(rho_f,3));
fprintf("P value value: %d\n",round(pval_f,3));
fprintf("N value is : %d\n",length(indt(~isnan(indt))));

%distance X
distance_x =All_bubble_data (:, 3);
indt = All_bubble_data (:, 28);
[rho_X,pval_X] = corr(distance_x,indt,'Type','Spearman','Rows','pairwise');
fprintf("Rho value: %d \n",round(rho_X,3));
fprintf("P value value: %d\n",round(pval_X,3));
fprintf("N value is : %d\n",length(indt(~isnan(indt))));

%distance Y
distance_y =All_bubble_data (:, 4);
indt = All_bubble_data (:, 28);
[rho_y,pval_y] = corr(distance_y,indt,'Type','Spearman','Rows','pairwise');
fprintf("Rho value: %d \n",round(rho_y,3));
fprintf("P value value: %d\n",round(pval_y,3));
fprintf("N value is : %d\n",length(indt(~isnan(indt))));

function se = stderr(data)
%% se = stderr(data)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% this returns std(data)/sqrt(length(data)-1);

    se = std(data,"omitmissing")./sqrt(sum(isfinite(data))-1);
end
