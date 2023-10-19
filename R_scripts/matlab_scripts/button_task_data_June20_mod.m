clear all , close all, clc, close all hidden
topLevelFolder = "/home/ananyapam/Projects/STREAM/data/START_sample_data";
fig_saving_folder = '/home/ananyapam/Projects/STREAM/data/figures';
output_folder = '/home/ananyapam/Projects/STREAM/data/Button_choice_task/output';
cd(topLevelFolder)
count = 0;
folders = [];
%filelist=dir('*.zip');

% Get list of all subfolders.
%allSubFolders = genpath(topLevelFolder);
% Parse into a cell array.
filelist = dir(topLevelFolder);
filelist(1:2) = [];
listOfFolderNames = {filelist.name};
listOfFolderNames = listOfFolderNames';

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
numberOfFolders = length(listOfFolderNames);

%% find group infor
clin_data = readtable('/home/ananyapam/Projects/STREAM/data/Clinical_data_new1.xls');

%% list of bad data from the observation at the time of testing
bad_data = [489,497,548,570,571,573,582,714,724,725,730,734,737]';
%indus_data = [477,478,480,484,485,486,487,491,493,495,498,500,501,502,503,510,517,537,538,540,543,544,545,546,549,550,552,553,555,556,557,558,559,560,564,565,566,567,568,572,575,576,577,578,579,581,627,647,655,658,664,665,666,667,668,672,673,674,675,676,677,678,679,680,681,682,685,686,687,688,689,690,691,693,694,695,696,699,701,702,703,704,706,708,709,710,711,712,715,716,717,718,719,720,721,722,723,726,727,728,729,733,736,738,739]';
for kk = 1 : numberOfFolders
    clear mat_fig
    thisFolder = listOfFolderNames{kk};% Get this folder and print it out.
    thisFolderPath = fullfile(topLevelFolder,thisFolder);        
    %% now do my bit
    cd(thisFolderPath)
    filePattern1 = sprintf('%s/*.xls', thisFolderPath);
    xlFileNames = dir(filePattern1);
    xlFileNames={xlFileNames.name};
    ix=regexp(xlFileNames,'choose_touching');
    ix=~cellfun('isempty',ix);

    if sum(ix)>0
        xlFileNames=xlFileNames(ix);
        xlFileNames = xlFileNames';

        clear filename1 txt
        filename1 = char(xlFileNames)

        %% find the last attempt made
        clear numbers 
        [~, sheets] = xlsfinfo(filename1);
        st =char(sheets(1,end));
        disp(st)
        [numbers, txt, everything] = xlsread(filename1,st); %% read xsl file
        %% disp(txt)
        h_values = txt(:,8);
        tf = contains(h_values,"silent");
        rf = contains(h_values,"social");
        disp(sum(tf));
        id = thisFolder; %get backend ID
        expression = 'child_(\d+)_';
        matches = regexp(id, expression, 'tokens');

        childID = str2double(matches{1}{1});
        
        if sum(tf)>1
            fprintf("folder %s has silent in it\n",thisFolder);
            ids(kk,1) = childID;          
        elseif sum(rf)>1
            disp("No silent");
            ids_non(kk,1) = childID;
        end  



       %% get the id
        id = thisFolder; %get backend ID
        ppt(kk,1) = childID;

        %% is it a bad data
        for pp = 1:length(bad_data)
            if ppt(kk,1)  == bad_data(pp,1)
                invalid_data(kk,1) = 1;
                break
            else
                invalid_data(kk,1) = 0;
            end
        end

        %% get the button press data
        green_click (kk,1)= numbers(1,1);
        red_click (kk,1)= numbers(3,1);
        interrupt (kk,1)= numbers(2,1);
        trials (kk,1) = green_click (kk,1) + red_click (kk,1);
        fprintf("total trials are :%d",trials(kk,1));
        if trials (kk,1)>= 1
            %% find out if red is social or non-social
            for jj = 23:length(txt) %% find out any red clicks
                if isempty(txt{jj,1}) == 1
                    1+1;
                else
                    if strcmp(txt(jj,1),'Red')== 1
                        if contains(txt(jj,8),'non') ==1
                            non_social(kk,1) = red_click(kk,1);
                            social(kk,1) = green_click(kk,1);
                        elseif contains(txt(jj,8),'social') ==1
                            non_social(kk,1) = green_click(kk,1) ;
                            social(kk,1) = red_click(kk,1);
                        end
                        break
                    elseif strcmp(txt(jj,1),'Green')== 1
                        if contains(txt(jj,8),'non')==1                        
                            non_social(kk,1) = green_click(kk,1) ;
                            social(kk,1) = red_click(kk,1);
                        elseif contains(txt(jj,8),'social') ==1
                            non_social(kk,1) = red_click(kk,1) ;
                            social(kk,1) = green_click(kk,1);
                        end
                        break
                    else
                        disp ('error')
                    end
                end
            end
        elseif trials (kk,1)<1
            disp ('missed data')
            social (kk,1) = nan;
            non_social (kk,1)= nan;
        end

    else
        social (kk,1) = nan;
        non_social (kk,1)= nan;
    end



end
    

    


%% put the data together
cd(output_folder)
Soc_prop= social./trials;%% convert social choice into proportion
All_button_data = [ppt, social, non_social, interrupt, trials, Soc_prop, clin_data, invalid_data];
histcounts(All_button_data(:,9))

%% remove any junk data
All_button_data(any(All_button_data(:,1)==0,2), : ) = [];  %remove rows with no ID
histcounts(All_button_data(:,9))
All_button_data(any(isnan(All_button_data(:,9)),2), : ) = [];  %remove rows with no diagnosis
histcounts(All_button_data(:,9))

%% write files for analysis



%% data cleaning
All_button_data(any(All_button_data(:,5)<4,2), : ) = [];  %remove participants with less than 4 valid trials
histcounts(All_button_data(:,9))
All_button_data(any(All_button_data(:,32)==1,2), : ) = [];  %remove participants with bad data
histcounts(All_button_data(:,9))
dlmwrite('button_data.txt',All_button_data,'\t')
xlswrite('button_data.xlsx',All_button_data)
csvwrite('button_data.csv',All_button_data)

%% group stats
cd("C:\idddp\START_ML\Old_start\START_India_data_analysis_sharing\Button_choice_task\Data_extraction")

contT = 1;
contA = 1;
contI = 1;
for RR = 1: length(All_button_data(:,1))
    if All_button_data(RR,9)==1
        soc_TD (contT,1) = All_button_data (RR, 6);
        contT = contT+1;
    elseif All_button_data(RR,9)==2
        soc_ASD (contA,1) =  All_button_data (RR, 6);
        contA = contA+1;
    elseif All_button_data(RR,9)==3
        soc_ID (contI,1) =  All_button_data (RR, 6);
        contI = contI+1;
    end
end

Msoc_TD = mean(soc_TD,'omitmissing');
SEsoc_TD = stderr(soc_TD);
Msoc_ASD = mean(soc_ASD,'omitmissing');
SEsoc_ASD = stderr(soc_ASD);
Msoc_ID = mean(soc_ID,'omitmissing');
SEsoc_ID = stderr(soc_ID);

social_pref  = [Msoc_TD, Msoc_ASD, Msoc_ID];
errors = [SEsoc_TD, SEsoc_ASD, SEsoc_ID];
dat = {soc_TD, soc_ASD, soc_ID};

figure(5)
bar(social_pref)
hold on
h = plotSpread(dat);
hold on
errorbar(social_pref, errors,'k','LineStyle', 'none','LineWidth',2)
% hold on
% plot([.5,3.5],[4,4],'k','LineWidth',2)
ylabel('Mean social preference','FontSize',16,'FontWeight', 'bold','LineWidth',2)
xlabel('Groups ','FontSize',16,'FontWeight', 'bold')
set(gca,'xtick',1:3, 'ytick',0:.25:1,'Ylim',[0,1],'FontSize',12, 'FontWeight', 'bold','LineWidth',2)
set(gca,'xticklabels',({'TD','ASD', 'ID'}))
title('Button task','FontSize',18,'FontWeight', 'bold')
set(gca,'box','off');
set(gcf,'Color',[1,1,1]);
set(h{1},'MarkerFaceColor',[0.85,0.33,0.10])
saveas(gcf,fullfile(fig_saving_folder,'social_groups'),'png')
saveas(gcf,fullfile(fig_saving_folder,'social_groups'),'fig')
saveas(gcf,fullfile(fig_saving_folder,'social_groups'),'eps')

%% do anova
[p,tbl,stats] = anova1(All_button_data(:,6),All_button_data(:,9))
[c,m,h,nms] = multcompare(stats)
disp("this is handle to the figure");
disp(h);
figure(4)
h = boxplot(All_button_data(:,6),All_button_data(:,9),'Notch','on','Labels',{'TD','ASD','ID'});
set(h,'LineWidth',2)
set(h(7,:),'MarkerSize',7)
ylabel('Proportion of choosing social button','FontSize',16,'FontWeight', 'bold')
set(gca,'box','off')
set(gca,'Color',[1,1,1]);
%xlabel('Groups ','FontSize',16,'FontWeight', 'bold')
title('Button task','FontSize',18,'FontWeight', 'bold')
set(gca,'xtick',1:3,'FontSize',12, 'FontWeight', 'bold','linewidth',3)
set(gca,'ytick',0:.25:1,'Ylim',[0,1],'FontSize',12, 'FontWeight', 'bold','linewidth',3)
set(gcf,'color','w')
saveas(gcf,fullfile(fig_saving_folder,'social_boxplot'),'png')
saveas(gcf,fullfile(fig_saving_folder,'social_boxplot'),'fig')
saveas(gcf,fullfile(fig_saving_folder,'social_boxplot'),'eps')

% corr 
social_choice =All_button_data (:, 6);
indt = All_button_data (:, 28);
[rho,pval] = corr(social_choice,indt,'Type','Spearman','Rows','pairwise');
fprintf("Rho value: %d \n",round(rho,3));
fprintf("P value value: %d\n",round(pval,3));
fprintf("N value is : %d\n",length(indt(~isnan(indt))));

function se = stderr(data)
%% se = stderr(data)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% this returns std(data)/sqrt(length(data)-1);

    se = std(data,"omitmissing")./sqrt(sum(isfinite(data))-1);
end