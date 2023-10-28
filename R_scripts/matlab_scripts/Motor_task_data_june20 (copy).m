clear all , close all, clc, close all hidden
import C:\idddp\START_ML\Old_start\gradients.*
% Define a starting folder.
topLevelFolder = "/home/ananyapam/Projects/STREAM/data/START_sample_data";
fig_saving_folder = 'C:\idddp\START_ML\Old_start\START_India_data_analysis_sharing\Motor_following_task\figures';
output_folder = 'C:\idddp\START_ML\Old_start\START_India_data_analysis_sharing\Motor_following_task\output';
cd(topLevelFolder)
filelist=dir(topLevelFolder);
filelist(1:2) = [];

% Get list of all subfolders.
%allSubFolders = genpath(topLevelFolder);
% Parse into a cell array.
listOfFolderNames = {filelist.name};

%remain = allSubFolders;
%while true
    %[singleSubFolder, remain] = strtok(remain, ';');
    %if isempty(singleSubFolder)
      %  break;
    %end
    %listOfFolderNames = [listOfFolderNames singleSubFolder];
%end
numberOfFolders = length(listOfFolderNames);

%% find group infor
clin_data = xlsread('C:\idddp\START_ML\Old_start\Clinical_data_new1.xlsx');
All_ppt_rmse = [];
rmse_mn= [];

%% list of bad data from the observation at the time of testing
bad_data = [553,556,557,562,578,582,656,668,685,690]';

for kk = 1 : numberOfFolders
    clear mat_fig diagnosis age gender
    
    thisFolder = listOfFolderNames{kk};
    
    thisFolderPath = fullfile(topLevelFolder, thisFolder);
    
    %% now do my bit
    cd(thisFolderPath)
    % get the data file
    %filePattern1 = sprintf('%s/*.xlsx', thisFolder);
    xlFileNames = dir(thisFolderPath);
    xlFileNames(1:2) = []
    xlFileNames={xlFileNames.name};
    ix=regexp(xlFileNames,'motoric');
    ix=~cellfun('isempty',ix);
    
    if sum(ix)>0
        
        xlFileNames=xlFileNames(ix);
        xlFileNames = xlFileNames';
        
        clear filename1 nunbers txt
        filename1 = char(xlFileNames)
        %% find the last attempt made by the child
        [~, sheets] = xlsfinfo(filename1);
        st =char(sheets(1,end));
        [beedata, txt, everything] = xlsread(filename1,st); %% read xsl file
        %% match the ids of the children
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
        
        %% adjust the number data to match same rows as text data
            add_n = zeros(12,14);
            mvmnts = [add_n;beedata];
           
        %% Find out number of trials completed
        attempt = find(strcmp(txt(:,1),'action'));
        numberOfTrials(kk,1) = length(attempt);
        
        for jj = 1:length(attempt)  %% get trial data
            clearvars data x bee_x y bee_y 
            if jj==1 && length(attempt)>1
                data = mvmnts(attempt(jj)+1:attempt(jj+1)-4,:);
            elseif jj==1 && length(attempt)==1
                data = mvmnts(attempt(jj)+1:end,:);
            elseif jj==2 && length(attempt)>2
                
                data = mvmnts(attempt(jj)+1:attempt(jj+1)-4,:);
            elseif jj==2 && length(attempt)==2
                data = mvmnts(attempt(jj)+1:end,:);
            elseif jj==3 && length(attempt)>3
                data = mvmnts(attempt(jj)+1:attempt(jj+1)-4,:);
            elseif jj==3 && length(attempt)==3
                data = mvmnts(attempt(jj)+1:end,:);
            elseif jj==4 && length(attempt)==4
                data = mvmnts(attempt(jj)+1:end,:);
            end
            
            %% use the data for fft analysis %%%%%%%%%%%%%%%
            bee_x = data(:,1);  % x_values for bee
            x = data(:,11);      % x_values for user
            if sum(x~=0)<5
                disp('very few touch points')
                rmse(kk,jj) = nan;
                weighted_x_freq_gain (kk,jj) = nan;
                weighted_y_freq_gain (kk,jj) = nan;
                speed_t (kk,jj) = nan;
                acceleration_t (kk,jj) = nan;
                jerk_t (kk,jj) = nan;
                
            else
                t = data(:,8);      % time_values (assumed millisecs)
                % Ignore the impact of delay in user's response. This is done by ignoring
                % all data prior to the first on screen contact by user. Comment this
                % section if not required
                first_non_zero_pos = find(x,1);
                bee_x = bee_x(first_non_zero_pos:end);
                x = x(first_non_zero_pos:end);
                t = t(first_non_zero_pos:end);
                
                % since time values are repeating, we find out distinct values of time
                % in the variable new_time
                [new_time, ~, ~] = unique(t);
                
                % for these unique time values we find out the average x values. e.g.
                % time t=274 repeats 5 times in the given data. The corresponding bee_x
                % values are 95,100,105,110,115. Therefore we arrive at a new bee_x which
                % is the average = 105. We do a similar averaging for user's x values and
                % store these averages in new_bee_x, for bee, and in new_x, for the user
                new_x = zeros(size(new_time));
                new_bee_x = zeros(size(new_time));
                sum_x = 0; count = 0; x_index = 0;
                sum_bee_x = 0;
                for i = 1:size(x)
                    sum_x = sum_x + x(i)
                    sum_bee_x = sum_bee_x + bee_x(i);
                    count = count + 1;
                    if ((i==size(x,1)) || (t(i)<t(i+1)))
                        
                        
                        fprintf("value of i is: %d",i);
                        %fprintf('index=%i\t; count=%i\t; ',index,count)
                        x_index = x_index + 1;
                        new_x(x_index)=sum_x/count;
                        new_bee_x(x_index)=sum_bee_x/count;
                        fprintf("\n this is xindex: %d newx(xindex) : %d sum_x = %d and count is : %d",x_index,new_x(x_index),sum_x, count);
                        sum_x=0;sum_bee_x=0;count=0;
                    end
                end
                
                %Since the samples are not taken at a constant interval of time we
                %interpolate the new_bee_x and new_x values over a new time range,
                %new_time_interp, which is spread evenly across the observed time window
                new_time_interp = linspace(new_time(1),new_time(end),size(new_time,1));
                new_time_interp = new_time_interp - new_time_interp(1); %shift time origin to t=0
                
                new_x_interp = interp1(new_time,new_x,linspace(new_time(1),new_time(end),size(new_time,1)));
                new_bee_x_interp = interp1(new_time,new_bee_x,linspace(new_time(1),new_time(end),size(new_time,1)));
                
                %mean shift the x values and find discrete FFT
                new_x_interp = new_x_interp - mean(new_x_interp);
                new_bee_x_interp = new_bee_x_interp - mean(new_bee_x_interp);
                n = size(new_time_interp,2);
                fft_x = fft(new_x_interp);
                fft_bee_x = fft(new_bee_x_interp);
                
                % consider only one half of the fft spectrum
                abs_fft_x = abs(fft_x/n);
                abs_fft_x = abs_fft_x(1:ceil(n/2));
                abs_fft_x(2:end-1) = 2*abs_fft_x(2:end-1);
                abs_fft_bee_x = abs(fft_bee_x/n);
                abs_fft_bee_x = abs_fft_bee_x(1:ceil(n/2));
                abs_fft_bee_x(2:end-1) = 2*abs_fft_bee_x(2:end-1);
                
                %% Repeat the above analysis for y-axis data
                clear new_y new_bee_y sum_y count y_index sum_bee_y new_time_interp
                clear new_bee_y_interp new_y_interp new_time t abs_fft_y fft_bee_y fft_bee n
                bee_y = data(:,3);
                y = data(:,13);
                if sum(y ~= 0)< 5 %% look out for low number of touch points
                    rmse(kk,:) = nan;
                    weighted_x_freq_gain (kk,jj) = nan;
                    weighted_y_freq_gain (kk,jj) = nan;
                    speed_t (kk,jj) = nan;
                    acceleration_t (kk,jj) = nan;
                    jerk_t (kk,jj) = nan;
                    
                else
                    t = data(:,8);
                    bee_y = bee_y(first_non_zero_pos:end);
                    y = y(first_non_zero_pos:end);
                    t = t(first_non_zero_pos:end);
                    
                    [new_time, ~, ~] = unique(t);
                    new_y = zeros(size(new_time));
                    new_bee_y = zeros(size(new_time));
                    sum_y = 0; count = 0; y_index = 0;
                    sum_bee_y = 0;
                    for i = 1:size(y)
                        sum_y = sum_y + y(i);
                        sum_bee_y = sum_bee_y + bee_y(i);
                        count = count + 1;
                        if ((i==size(y,1)) || (t(i)<t(i+1)))
                            %fprintf('index=%i\t; count=%i\t; ',index,count)
                            y_index = y_index + 1;
                            new_y(y_index)=sum_y/count;
                            new_bee_y(y_index)=sum_bee_y/count;
                            sum_y=0;sum_bee_y=0;count=0;
                        end
                    end
                    
                    %linearly interpolate the samples+
                    
                    new_time_interp = linspace(new_time(1),new_time(end),size(new_time,1));
                    new_time_interp = new_time_interp - new_time_interp(1); %shift time origin to t=0
                    
                    new_y_interp = interp1(new_time,new_y,linspace(new_time(1),new_time(end),size(new_time,1)));
                    new_bee_y_interp = interp1(new_time,new_bee_y,linspace(new_time(1),new_time(end),size(new_time,1)));
                    
                    %mean shift the interpolated data to ignore the dc component of fft
                    new_y_interp = new_y_interp - mean(new_y_interp);
                    new_bee_y_interp = new_bee_y_interp - mean(new_bee_y_interp);
                    
                    n = size(new_time_interp,2);
                    fft_y = fft(new_y_interp);
                    fft_bee_y = fft(new_bee_y_interp);
                    
                    abs_fft_y = abs(fft_y/n);
                    abs_fft_y = abs_fft_y(1:ceil(n/2));
                    abs_fft_y(2:end-1) = 2*abs_fft_y(2:end-1);
                    abs_fft_bee_y = abs(fft_bee_y/n);
                    abs_fft_bee_y = abs_fft_bee_y(1:ceil(n/2));
                    abs_fft_bee_y(2:end-1) = 2*abs_fft_bee_y(2:end-1);
                    
                    %% Metric calculation
                    x_freq_gain_vector = zeros(1,size(abs_fft_bee_x,2)-2);
                    y_freq_gain_vector = zeros(1,size(abs_fft_bee_y,2)-2);
                    for i = 2:size((abs_fft_bee_x),2)-1
                        avg_x_amp = mean(abs_fft_x(i-1:i+1));
                        avg_y_amp = mean(abs_fft_y(i-1:i+1));
                        x_freq_gain_vector(i-1) = avg_x_amp/abs_fft_bee_x(i);
                        y_freq_gain_vector(i-1) = avg_y_amp/abs_fft_bee_y(i);
                    end
                    
                    % final calculations stored in these variables
                    x_freq_gain_final = mean(x_freq_gain_vector);
                    y_freq_gain_final = mean(y_freq_gain_vector);
                    % adding amplitude bias to the calculations. Higher amplitudes get more
                    % weightage in the gain calculations
                    weighted_x_freq_gain (kk,jj) = sum(abs_fft_x(2:end-1).* x_freq_gain_vector) / sum(abs_fft_x(2:end-1));
                    weighted_y_freq_gain (kk,jj) = sum(abs_fft_y(2:end-1).* y_freq_gain_vector) / sum(abs_fft_y(2:end-1));
                    
                    
                    %% RMSE summate the data
                    data1 = [new_time,new_x,new_y];
                    estimate1 = [new_time,new_bee_x, new_bee_y];
                    matc = [data1,estimate1];
                    matc(any(matc(:,2)==0,2), : ) = [];
                    data2 = matc(:,2:3);
                    estimate2 = matc(:,5:6);
                    rmse(kk,jj)=sqrt(sum((estimate2(:)-data2(:)).^2)/numel(data2));
                    
                    %% now do the gradients
                    cd("C:\idddp\START_ML\Old_start\START_India_data_analysis_sharing\Motor_following_task\Data_extraction")
                    [grad1, grad2, grad3] = gradients(new_x,new_y,new_time);
                    speed_t(kk,jj) = mean(grad1,'omitmissing');
                    acceleration_t(kk,jj) = mean(grad2,'omitmissing');
                    jerk_t(kk,jj) = mean(grad3,'omitmissing');
                    
                end
            end
%             %% now check the quality of the data
%             if jj ==1
%                 Qual_data = data1(1:100,:);% take 100 touch points
%                 Qual_estimate = estimate1(1:100,:);
%                 Qual = Qual_estimate - Qual_data;
%                 Qual_S = Qual .^2;
%                 Qual_NS = sqrt(Qual_S);
%                 Qual_mn = nanmean(Qual_NS,1);
%                 if Qual_mn(1,2)> 200 && Qual_mn(1,3)> 200 %% check if participant was even trying to follow the target by being within 200 pixels
%                     Data_quality (kk,1) = 0;
%                 else
%                     Data_quality (kk,1) = 1;
%                 end
%             end
        end
        
        %% get mean over the number of trials
        rmse_mn(kk,1) = nansum(rmse(kk,:))/length(attempt);
        weighted_x_freq_gain_mn(kk,1)= nansum(weighted_x_freq_gain (kk,:))/length(attempt);
        weighted_y_freq_gain_mn(kk,1)= nansum(weighted_y_freq_gain (kk,:))/length(attempt);
        speed(kk,1) = nansum(speed_t(kk,:))/length(attempt);
        acceleration(kk,1) = nansum(acceleration_t(kk,:))/length(attempt);
        jerk(kk,1) = nansum(jerk_t(kk,:))/length(attempt);
        
        %% put all the data together
        All_motor_data(kk,:) = [ppt(kk,1), rmse_mn(kk,1),numberOfTrials(kk,1),...
            weighted_x_freq_gain_mn(kk,1), weighted_y_freq_gain_mn(kk,1)...
            speed(kk,1), acceleration(kk,1), jerk(kk,1), clinical_data(kk,:),invalid_data(kk,1)];
        
    end
    
end

cd(output_folder)
histcounts(All_motor_data(:,11))
All_motor_data(any(isnan(All_motor_data(:,1)),2), : ) = [];  %remove rows with no ID
All_motor_data(any(All_motor_data(:,1)==0,2), : ) = [];  %remove rows with no ID
histcounts(All_motor_data(:,11))
All_motor_data(any(isnan(All_motor_data(:,11)),2), : ) = [];  %remove rows with no diagnosis
All_motor_data(any(All_motor_data(:,11)==0,2), : ) = [];  %remove rows with zero diagnosis
histcounts(All_motor_data(:,11))
All_motor_data(any(All_motor_data(:,34)==1,2), : ) = [];  %remove rows with bad data
histcounts(All_motor_data(:,11))



%% remove bad data
%All_motor_data(any(All_motor_data(:,29)==0,2), : ) = [];  %remove rows with less tha 1/3 of good attempt in first trial
All_motor_data(any(All_motor_data(:,3)<2 ,2), : ) = [];  %remove rows with less than 2 trials
histcounts(All_motor_data(:,11))

%remove any values that are inf
All_motor_data(any(All_motor_data(:)==Inf,2))= NaN;

dlmwrite('motor_data.txt',All_motor_data,'\t')
xlswrite('motor_data.xlsx',All_motor_data)
csvwrite('motor_data.csv',All_motor_data)

%% group stats
cd("C:\idddp\START_ML\Old_start\START_India_data_analysis_sharing\Motor_following_task\Data_extraction")
contT = 1;
contA = 1;
contI = 1;
for RR = 1: length(All_motor_data(:,1))
    if All_motor_data(RR,11)==1
        rmse_TD (contT,1) = All_motor_data (RR, 2);
        fftX_TD (contT,1) = All_motor_data (RR, 4);
        fftY_TD (contT,1) = All_motor_data (RR, 5);
        jerk_TD (contT,1) = All_motor_data (RR, 8);
        contT = contT+1;
    elseif All_motor_data(RR,11)==2
        rmse_ASD (contA,1) =  All_motor_data (RR, 2);
        fftX_ASD (contA,1) = All_motor_data (RR, 4);
        fftY_ASD (contA,1) = All_motor_data (RR, 5);
        jerk_ASD (contA,1) = All_motor_data (RR, 8);
        contA = contA+1;
    elseif All_motor_data(RR,11)==3
        rmse_ID (contI,1) =  All_motor_data (RR, 2);
        fftX_ID (contI,1) = All_motor_data (RR, 4);
        fftY_ID (contI,1) = All_motor_data (RR, 5);
        jerk_ID (contI,1) = All_motor_data (RR, 8);
        contI = contI+1;
    end
end

motor_rmse  = [mean(rmse_TD,'omitmissing'), mean(rmse_ASD,'omitmissing'), mean(rmse_ID,'omitmissing')];
errors_rmse = [stderr(rmse_TD), stderr(rmse_ASD), stderr(rmse_ID)];
motor_fftX  = [mean(fftX_TD,'omitmissing'), mean(fftX_ASD,'omitmissing'), mean(fftX_ID,'omitmissing')];
errors_fftX = [stderr(fftX_TD), stderr(fftX_ASD), stderr(fftX_ID)];
motor_fftY  = [mean(fftY_TD,'omitmissing'), mean(fftY_ASD,'omitmissing'), mean(fftY_ID,'omitmissing')];
errors_fftY = [stderr(fftY_TD), stderr(fftY_ASD), stderr(fftY_ID)];
motor_jerk  = [mean(jerk_TD,'omitmissing'), mean(jerk_ASD,'omitmissing'), mean(jerk_ID,'omitmissing')];
errors_jerk = [stderr(jerk_TD), stderr(jerk_ASD), stderr(jerk_ID)];

dat_rmse = {rmse_TD, rmse_ASD, rmse_ID};
dat_fftX = {fftX_TD, fftX_ASD, fftX_ID};
dat_fftY = {fftY_TD, fftY_ASD, fftY_ID};
dat_jerk = {jerk_TD, jerk_ASD, jerk_ID};


%% do anova
[prmse,~,statsrmse] = anova1(All_motor_data(:,2),All_motor_data(:,11)) %for rmse
[pfftx,~,statsfftx] = anova1(All_motor_data(:,4),All_motor_data(:,11)) % for fft x
[pffty,~,statsffty] = anova1(All_motor_data(:,5),All_motor_data(:,11)) %for fft y
[pjerk,~,statsjerk] = anova1(All_motor_data(:,8),All_motor_data(:,11)) % for jerk

%{
figure(15)
bar(motor_rmse)
hold on
h = plotSpread(dat_rmse);
hold on
errorbar(motor_rmse, errors_rmse,'k','LineStyle', 'none','LineWidth',2)
ylabel('Mean RMSE','FontSize',16,'FontWeight', 'bold','LineWidth',2)
set(gca,'xtick',1:3,'FontSize',12, 'FontWeight', 'bold','LineWidth',2)
set(gca,'xticklabels',({'TD','ASD', 'ID'}))
title('Motor task: RMSE','FontSize',18,'FontWeight', 'bold')
set(gca,'box','off');
set(gcf,'Color',[1,1,1]);
set(h{1},'MarkerFaceColor',[0.85,0.33,0.10])
saveas(gcf,fullfile(fig_saving_folder,'rmse_groups'),'png')

figure(16)
bar(motor_fftX)
hold on
h = plotSpread(dat_fftX);
hold on
errorbar(motor_fftX, errors_fftX,'k','LineStyle', 'none','LineWidth',2)
ylabel('Weighted fft gain X axis','FontSize',16,'FontWeight', 'bold','LineWidth',2)
set(gca,'xtick',1:3,'FontSize',12, 'FontWeight', 'bold','LineWidth',2)
set(gca,'xticklabels',({'TD','ASD', 'ID'}))
title('Motor task: FFT X axis','FontSize',18,'FontWeight', 'bold')
set(gca,'box','off');
set(gcf,'Color',[1,1,1]);
set(h{1},'MarkerFaceColor',[0.85,0.33,0.10])
saveas(gcf,fullfile(fig_saving_folder,'fftx_groups'),'png')

figure(17)
bar(motor_fftY)
hold on
h = plotSpread(dat_fftY);
hold on
errorbar(motor_fftY, errors_fftY,'k','LineStyle', 'none','LineWidth',2)
ylabel('Weighted fft gain Y axis','FontSize',16,'FontWeight', 'bold','LineWidth',2)
set(gca,'xtick',1:3,'FontSize',12, 'FontWeight', 'bold','LineWidth',2)
set(gca,'xticklabels',({'TD','ASD', 'ID'}))
title('Motor task: FFT Y Axis','FontSize',18,'FontWeight', 'bold')
set(gca,'box','off');
set(gcf,'Color',[1,1,1]);
set(h{1},'MarkerFaceColor',[0.85,0.33,0.10])
saveas(gcf,fullfile(fig_saving_folder,'ffty_groups'),'png')

figure(18)
bar(motor_jerk)
hold on
h = plotSpread(dat_jerk);
hold on
errorbar(motor_jerk, errors_jerk,'k','LineStyle', 'none','LineWidth',2)
ylabel('Mean jerk','FontSize',16,'FontWeight', 'bold','LineWidth',2)
set(gca,'xtick',1:3,'FontSize',12, 'FontWeight', 'bold','LineWidth',2)
set(gca,'xticklabels',({'TD','ASD', 'ID'}))
title('Motor task: Jerk','FontSize',18,'FontWeight', 'bold')
set(gca,'box','off');
set(gcf,'Color',[1,1,1]);
set(h{1},'MarkerFaceColor',[0.85,0.33,0.10])
saveas(gcf,fullfile(fig_saving_folder,'jerk_groups'),'png')

%}
function se = stderr(data)
%% se = stderr(data)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% this returns std(data)/sqrt(length(data)-1);

    se = std(data,"omitmissing")./sqrt(sum(isfinite(data))-1);
end