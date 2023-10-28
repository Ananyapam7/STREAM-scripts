clear all , close all, clc, clf, close all hidden


topLevelFolder = "C:\idddp\START_ML\sample";
fig_saving_folder = 'C:\idddp\START_ML\Old_start\START_India_data_analysis_sharing\Colouring_task\figures';
cd(topLevelFolder)

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
clin_data = xlsread('C:\idddp\START_ML\Old_start\Clinical_data_new1.xlsx');

%% list of bad data from the observation at the time of testing
bad_data = [479,544,571,572,580,582,655,656,668,704,708]';
% files 554 seems to have some problem
% image files of 479 are currpt

interrupt = [];
for k = 1: numberOfFolders
    clear mat_fig attempt
    
    % Get this folder and print it out.
    thisFolder = listOfFolderNames{k}
    thisFolderPath = fullfile(topLevelFolder, thisFolder);
    
    %% now do my bit
    cd(thisFolderPath)
    % get colouring data file
    filePattern1 = sprintf('%s/*.xlsx', thisFolderPath);
    xlFileNames = dir(filePattern1);
    xlFileNames={xlFileNames.name};
    ix=regexp(xlFileNames,'coloring');
    ix=~cellfun('isempty',ix);
    
    if sum(ix)>0
        
        xlFileNames=xlFileNames(ix);
        xlFileNames = xlFileNames';
        
        clear filename1 txt numbers

        filename1 = char(xlFileNames);
        %% find the last attempt made by the child
        [~, sheets] = xlsfinfo(filename1);
        
         if ~isempty(find(strcmp(sheets,'Attempt #3')))
            sht = find(strcmp(sheets,'Attempt #3'));
        elseif ~isempty(find(strcmp(sheets,'Attempt #2')))
            sht = find(strcmp(sheets,'Attempt #2'));
        elseif ~isempty(find(strcmp(sheets,'Attempt #1')))
            sht = find(strcmp(sheets,'Attempt #1'));
        end
              
        st =char(sheets(sht));
        [mov_data, txt, everything] = xlsread(filename1,st); %% read xsl file
        
        last_attempt(k,1)= {st};
        
        %% match the ids of the children
        id = thisFolder; %get backend ID
        expression = 'child_(\d+)_';
        matches = regexp(id, expression, 'tokens');

        childID = str2double(matches{1}{1});
        ppt(k,1) = childID;

        %% match the clinical data
        for pp = 1:length(clin_data)
            if ppt(k,1)  == clin_data(pp,1)
                clinical_data(k,1:25) = clin_data(pp,1:25);%% 3=diagnosis, 4=vsms, 15=DQ, 19=cog_age, 22=INDT, 23=age, 24=gender, 25=diagnosis with ID_ASD as 4
                break
            else
                clinical_data(k,1:25) = nan(1,25);
            end
        end
        
         %% is it a bad data
        for pp = 1:length(bad_data)
            if ppt(k,1)  == bad_data(pp,1)
                invalid_data(k,1) = 1;
                break
            else
                invalid_data(k,1) = 0;
            end
        end
                
        %% adjust data size
        ll = size(mov_data);
        wdth = ll(1,2);
        add_n = zeros(12,wdth);
        mov_data = [add_n;mov_data];
        mov_data(1:12,:) =nan;
        
        %% find out if the task was aborted
         interrupt(k,1) = mov_data(13,2);
        
            %% get child's data
            for ww = 1:2
                gen_name = strcat('Sub-attempt #',num2str(ww));
                for pn = 1:length(txt)
                    rmatch = strcmp(txt(pn,1),gen_name);
                    counter = 1;
                    if rmatch == 1
                        attempt(ww) = pn;
                        
                    end
                end
            end
            
            if length(attempt)== 2
                mvmnts_first = mov_data(attempt(1)+3:attempt(2)-2,:);
                mvmnts_second = mov_data(attempt(2)+3:end,:);
            elseif length(attempt)==1
                mvmnts_first = mov_data(attempt(1)+3:end,:);
            end
            
            
            %% get coordinates
            for ww = 1:length(attempt)
                if ww== 1
                    try
                    coordinates_first = xlsread(filename1,sht+2);
                    catch me
                    disp(me)
                    end
                elseif ww==2
                    try
                    coordinates_second = xlsread(filename1,sht+4);
                    catch me
                    disp(me)
                    end
                end
            end
            
            %% now calculate areas
            for aa = 1:length(attempt)
                clear mvmnts xa xb ya yb
                if aa== 1
                    mvmnts = mvmnts_first;  %% separate data of two attempts
                    coordinate1 = coordinates_first;
                elseif aa== 2
                    mvmnts = mvmnts_second;
                    coordinate1 = coordinates_second;
                end
                
                xv = coordinate1(:,1);
                yv = coordinate1(:,2);
                xq = round(mvmnts(:,8));
                yq = round(mvmnts(:,10));
                
                
                %% get value
                in = inpolygon(xq,yq,xv,yv);
                inside(aa)= numel(xq(in));
                outside(aa)= numel(xq(~in));
                              
                %% find time points
                crossover = diff(in(:));
                t = cumsum([true;diff(crossover)~=0]);
                t1 = t(crossover~=0);
                t21 = unique(t1);
                crossover1(aa) = numel(t21); 
                cross_time = mvmnts(t21,5);
                dur = diff(cross_time);
                cross_dur(aa) = sum(dur);
                
            end
            
            %% put data together
            inside1 = mean(inside);
            outside1 = mean(outside);
            crossover2 = mean(crossover1);
            cross_dur1 = mean(cross_dur);
            ppt_data(k,:) = [inside1,outside1,crossover2,cross_dur1]  ;            
        
    else
        ppt_data(k,:) = [nan, nan, nan, nan]  ;
        fprintf('Folder %s has no image files in it.\n', thisFolder);
        
    end
    
    %%%%%%%%%% do the image based analysis
    %% now do my bit
    cd(thisFolderPath)
    % get colouring data file
    filePattern2 = sprintf('%s/*.jpg', thisFolderPath);
    xlFileNamesI = dir(filePattern2);
    xlFileNamesI={xlFileNamesI.name};
    ixI=regexp(xlFileNamesI,'coloring');
    ixI=~cellfun('isempty',ixI);
    
    if sum(ixI)>0
        xlFileNamesI=xlFileNamesI(ixI);
        xlFileNamesI = xlFileNamesI';
        fls = length(xlFileNamesI);
        if fls >=2
            flsn = 2;
        elseif fls == 1
            flsn = 1;
        end
        for dd = 1:flsn
            clear filename1 prop_col prop_Ncol counts all_pix flsq
            if dd == 1
                flsq = fls;% find the files frm last attempt
            else
                flsq = fls-1;
            end
            filename2 = char(xlFileNamesI(flsq,1));
            try
                data_out = im2bw(rgb2gray(imread(filename2)),0.95);
            catch me
                disp(id)
                disp(me)
            end 
            counts = imhist(data_out); % col 1 colured pixels and col 2 non-coloured
            all_pix = sum(counts);
            prop_col = counts(1,1)/all_pix;
            pix_data(k,dd) = prop_col;
        end
    else
        disp ('missing image files')
        pix_data(k,1:34) = nan(1,34);
    end
    
    %%%%%%%
    
end
cd("C:\idddp\START_ML\Old_start\START_India_data_analysis_sharing\Colouring_task\output")


format shortG
All_colour_data = [ppt, ppt_data, interrupt, clinical_data, pix_data,invalid_data];
histcounts(All_colour_data(:,9))
All_colour_data(any(isnan(All_colour_data(:,1)),2), : ) = [];  %remove rows with no ID
All_colour_data(any(All_colour_data(:,1)==0,2), : ) = [];  %remove rows with no ID
histcounts(All_colour_data(:,9))
All_colour_data(any(isnan(All_colour_data(:,9)),2), : ) = [];  %remove rows with no diagnosis
All_colour_data(any(All_colour_data(:,9)==0,2), : ) = [];  %remove rows with zero diagnosis
histcounts(All_colour_data(:,9))
All_colour_data(any(All_colour_data(:,34)==1,2), : ) = [];  %remove rows with bad data
histcounts(All_colour_data(:,9))


save colour_data.mat All_colour_data
dlmwrite('colour_data.txt',All_colour_data,'\t')
xlswrite('colour_data.xlsx',All_colour_data)
csvwrite('colour_data.csv',All_colour_data)


%% remove data with less than 25% colured pixels
All_colour_data(any(All_colour_data(:,32)<.25,2), : ) = [];  %poor data quality
All_colour_data(any(All_colour_data(:,33)<.25,2), : ) = [];  %poor data quality
histcounts(All_colour_data(:,9))

%% make group stats
cd("C:\idddp\START_ML\Old_start\START_India_data_analysis_sharing\Colouring_task\Data_extraction")
contT = 1;
contA = 1;
contI = 1;
for RR = 1: length(All_colour_data(:,1))
    if All_colour_data(RR,9)==1
        col_TD (contT,1) = All_colour_data (RR, 4);
        contT = contT+1;
    elseif All_colour_data(RR,9)==2
        col_ASD (contA,1) =  All_colour_data (RR, 4);
        contA = contA+1;
    elseif All_colour_data(RR,9)==3
        col_ID (contI,1) =  All_colour_data (RR, 4);
        contI = contI+1;
    end
end

M_TD = mean(col_TD,"omitmissing");
SE_TD = stderr(col_TD);
M_ASD = mean(col_ASD,"omitmissing");
SE_ASD = stderr(col_ASD);
M_ID = mean(col_ID,"omitmissing");
SE_ID = stderr(col_ID);

colour_out  = [M_TD, M_ASD, M_ID];
errors = [SE_TD, SE_ASD, SE_ID];
dat = {col_TD, col_ASD, col_ID};

figure(4)
bar(colour_out)
hold on
h = plotSpread(dat);
hold on
errorbar(colour_out, errors,'k','LineStyle', 'none','LineWidth',2)
ylabel('Crossing over the lines','FontSize',16,'FontWeight', 'bold','LineWidth',2)
%xlabel('Groups ','FontSize',16,'FontWeight', 'bold')
set(gca,'xtick',1:3,'FontSize',12, 'FontWeight', 'bold','LineWidth',2)
set(gca,'xticklabels',({'TD','ASD', 'ID'}))
title('Colouring task','FontSize',18,'FontWeight', 'bold')
set(gca,'box','off');
set(gcf,'Color',[1,1,1]);
set(h{1},'MarkerFaceColor',[0.85,0.33,0.10])
saveas(gcf,fullfile(fig_saving_folder,'colour_groups'),'png')
saveas(gcf,fullfile(fig_saving_folder,'colour_groups'),'fig')
saveas(gcf,fullfile(fig_saving_folder,'colour_groups'),'eps')

%% do anova
[p,tbl,stats] = anova1(All_colour_data(:,4),All_colour_data(:,9))
[c,m,h,nms] = multcompare(stats)

figure(5)
h = boxplot(All_colour_data(:,4),All_colour_data(:,9),'Notch','on','Labels',{'TD','ASD','ID'});
set(h,'LineWidth',2)
set(h(7,:),'MarkerSize',7)
ylabel('Crossing over the lines','FontSize',16,'FontWeight', 'bold')
set(gca,'box','off')
set(gca,'Color',[1,1,1]);
xlabel('Groups ','FontSize',16,'FontWeight', 'bold')
title('Colouring task','FontSize',18,'FontWeight', 'bold')
set(gca,'xtick',1:3,'FontSize',12, 'FontWeight', 'bold','linewidth',3)
%set(gca,'ytick',0:.25:1,'Ylim',[0,1],'FontSize',12, 'FontWeight', 'bold','linewidth',3)
set(gcf,'color','w')
saveas(gcf,fullfile(fig_saving_folder,'boxplot'),'png')
saveas(gcf,fullfile(fig_saving_folder,'boxplot'),'fig')
saveas(gcf,fullfile(fig_saving_folder,'boxplot'),'eps')


cross =All_colour_data (:, 4);
indt = All_colour_data (:, 28);
[rho,pval] = corr(cross,indt,'Type','Spearman','Rows','pairwise');
fprintf("Rho value: %d \n",round(rho,3));
fprintf("P value value: %d\n",round(pval,3));
fprintf("N value is : %d\n",length(indt(~isnan(indt))));
function se = stderr(data)
%% se = stderr(data)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% this returns std(data)/sqrt(length(data)-1);

    se = std(data,"omitmissing")./sqrt(sum(isfinite(data))-1);
end
