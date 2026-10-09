function onOpen() {
  SpreadsheetApp.getUi().createMenu('4th Down').addItem('Calculate', 'calculateFourthDown').addItem('Check results', 'checkFourthDown').addItem('Connect GitHub', 'configureFourthDown').addToUi();
}
function configureFourthDown() {
  const ui=SpreadsheetApp.getUi(), p=PropertiesService.getScriptProperties();
  const r=ui.prompt('GitHub repository','Enter OWNER/REPOSITORY for your public calculator repository.',ui.ButtonSet.OK_CANCEL);
  if(r.getSelectedButton()!==ui.Button.OK)return;
  if(!/^[A-Za-z0-9_.-]+\/[A-Za-z0-9_.-]+$/.test(r.getResponseText().trim()))throw Error('Use OWNER/REPOSITORY.');
  const t=ui.prompt('GitHub token','Enter a fine-grained token for this repository with Actions read/write permission. Stored in script properties.',ui.ButtonSet.OK_CANCEL);
  if(t.getSelectedButton()!==ui.Button.OK)return;
  if(!t.getResponseText().trim())throw Error('Token required.');
  p.setProperties({GH_REPO:r.getResponseText().trim(),GH_TOKEN:t.getResponseText().trim()});
}
function gh_(path,method,payload,raw) {
  const p=PropertiesService.getScriptProperties(),repo=p.getProperty('GH_REPO'),token=p.getProperty('GH_TOKEN');
  if(!repo||!token)throw Error('Use 4th Down → Connect GitHub first.');
  const options={method:method||'get',headers:{Authorization:'Bearer '+token,Accept:'application/vnd.github+json','X-GitHub-Api-Version':'2026-03-10'},muteHttpExceptions:true,followRedirects:!raw};
  if(payload){options.contentType='application/json';options.payload=JSON.stringify(payload);}
  const response=UrlFetchApp.fetch('https://api.github.com/repos/'+repo+path,options);
  const status=response.getResponseCode();
  if(status<200||status>=400)throw Error('GitHub HTTP '+status+'. Check repository, token permissions and Actions run.');
  return raw?response:(response.getContentText()?JSON.parse(response.getContentText()):{});
}
function invalidateFourthDown_(s,message) {
  s.getRange('F3').setValue('Not calculated');s.getRange('F5:G7').clearContent();s.getRange('F8:F9').clearContent();s.getRange('F2').setValue(message);
}
function onEdit(e) {
  if(!e||e.range.getSheet().getName()!=='Calculator')return;
  const r=e.range;
  if(r.getColumn()<=2&&r.getLastColumn()>=2&&r.getRow()<=24&&r.getLastRow()>=4)invalidateFourthDown_(r.getSheet(),'Inputs changed — calculate again');
}
function calculateFourthDown() {
  const lock=LockService.getDocumentLock();if(!lock.tryLock(1000))return;
  const s=SpreadsheetApp.getActive().getSheetByName('Calculator');
  try {
    const props=PropertiesService.getScriptProperties();
    if(props.getProperty('PENDING_RUN'))throw Error('A run is pending. Use Check results before starting another.');
    invalidateFourthDown_(s,'Submitting to GitHub…');
    const v=s.getRange('B4:B24').getValues().flat();
    const n=(i,name,min,max)=>{ const x=v[i]; if (x==='' || typeof x!=='number' || !Number.isFinite(x) || x<min || x>max) throw new Error(name+' is invalid.'); return x; };
    const integer=(i,name,min,max)=>{const x=n(i,name,min,max);if (!Number.isInteger(x)) throw new Error(name+' must be a whole number.');return x;};
    const clock=s.getRange('B17').getDisplayValue().trim();
    const m=clock.match(/^(\d{1,2}):([0-5]\d)$/);
    if (!m) throw new Error('Clock must be MM:SS, such as 02:09.');
    const seconds=60*Number(m[1])+Number(m[2]);
    if (seconds<1 || seconds>900) throw new Error('Clock must be between 00:01 and 15:00.');
    if (v[1]===v[2]) throw new Error('Team and opponent must differ.');
    const yard=integer(8,'Yard line',1,50);
    const distance=v[7]==='Own'?100-yard:yard;
    const payload={mode:v[0],posteam:v[1],opponent:v[2],is_home:v[3]==='Home',
      season:integer(4,'Season',2014,2100),type:v[5],qtr:integer(12,'Quarter',1,4),
      quarter_seconds_remaining:seconds,yardline_100:distance,ydstogo:integer(9,'Yards to go',1,distance),
      score_differential:integer(10,'Team score',0,150)-integer(11,'Opponent score',0,150),
      posteam_timeouts_remaining:integer(14,'Team timeouts',0,3),
      defteam_timeouts_remaining:integer(15,'Opponent timeouts',0,3),
      possession_received_kickoff:v[16]==='Possession team'};
    if (payload.mode==='Hypothetical') {
      payload.expected_margin=n(18,'Expected margin',-100,100);
      payload.total_line=n(19,'Expected combined points',1,200);
      payload.roof=v[20];
      if (payload.total_line<=Math.abs(payload.expected_margin)) throw new Error('Total points must exceed the absolute expected margin.');
    }
    const snapshot=JSON.stringify(s.getRange('B4:B24').getValues());

    const response=gh_('/actions/workflows/calculate.yml/dispatches','post',{ref:'main',inputs:{situation:JSON.stringify(payload)}});
    if(!response.workflow_run_id)throw Error('GitHub did not return the run ID.');
    props.setProperty('PENDING_RUN',JSON.stringify({id:response.workflow_run_id,snapshot:snapshot,sheetId:SpreadsheetApp.getActive().getId(),mode:payload.mode,started:Date.now()}));
    ensurePoll_();
    s.getRange('F2').setValue('Queued on GitHub • results will update automatically');
  } catch(err){s.getRange('F2').setValue('Error: '+err.message);SpreadsheetApp.getActive().toast(err.message,'4th Down',10);}
  finally{lock.releaseLock();}
}
function ensurePoll_() {
  if(!ScriptApp.getProjectTriggers().some(t=>t.getHandlerFunction()==='checkFourthDown'))ScriptApp.newTrigger('checkFourthDown').timeBased().everyMinutes(1).create();
}
function stopPoll_() {
  ScriptApp.getProjectTriggers().filter(t=>t.getHandlerFunction()==='checkFourthDown').forEach(t=>ScriptApp.deleteTrigger(t));
}
function checkFourthDown() {
  const lock=LockService.getDocumentLock();if(!lock.tryLock(1000))return;
  const props=PropertiesService.getScriptProperties();
  let s;
  try {
    const text=props.getProperty('PENDING_RUN');if(!text){stopPoll_();return;}
    const pending=JSON.parse(text);
    s=SpreadsheetApp.openById(pending.sheetId).getSheetByName('Calculator');
    if(Date.now()-pending.started>30*60*1000)throw Error('Run timed out after 30 minutes. Check GitHub Actions.');
    const run=gh_('/actions/runs/'+pending.id);
    if(run.status!=='completed'){s.getRange('F2').setValue('GitHub: '+run.status);return;}
    if(run.conclusion!=='success')throw Error('GitHub run '+run.conclusion+'. Check its Actions log.');
    const artifacts=gh_('/actions/runs/'+pending.id+'/artifacts').artifacts;
    const artifact=artifacts.find(a=>a.name==='result'&&!a.expired);
    if(!artifact)throw Error('Result artifact missing.');
    const redirect=gh_('/actions/artifacts/'+artifact.id+'/zip','get',null,true);
    const headers=redirect.getAllHeaders(), location=headers.Location||headers.location;
    // Never forward GitHub credentials to the temporary storage host.
    const archive=location?UrlFetchApp.fetch(location).getBlob():redirect.getBlob();
    const file=Utilities.unzip(archive).find(b=>b.getName()==='result.json');
    if(!file)throw Error('result.json missing.');
    const result=JSON.parse(file.getDataAsString());
    if(result.error)throw Error(result.error);
    if(pending.snapshot!==JSON.stringify(s.getRange('B4:B24').getValues())){invalidateFourthDown_(s,'Inputs changed — calculate again');props.deleteProperty('PENDING_RUN');stopPoll_();return;}
    const payload={mode:pending.mode};
    const choices=result.choices;
    if (!Array.isArray(choices) || choices.length!==3) throw new Error('Invalid model response.');
    const probability=x=>{if(x===null)return 'N/A';if(typeof x!=='number'||!Number.isFinite(x)||x<0||x>1)throw new Error('Invalid probability.');return x;};
    const values=choices.map(x=>[probability(x.win_probability),probability(x.success_probability)]);
    if(typeof result.advantage_pp!=='number'||!Number.isFinite(result.advantage_pp))throw new Error('Invalid model advantage.');
    s.getRange('F5:G7').setValues(values).setNumberFormat('0.0%');
    s.getRange('F3').setValue(result.recommendation);
    s.getRange('F8').setValue(result.advantage_pp).setNumberFormat('0.00');
    s.getRange('F9').setValue(new Date()).setNumberFormat('yyyy-mm-dd hh:mm:ss');
    s.getRange('F2').setValue('Calculated • '+payload.mode);

    props.deleteProperty('PENDING_RUN');stopPoll_();
  } catch(err) {
    if(s)invalidateFourthDown_(s,'Error: '+err.message);
    props.deleteProperty('PENDING_RUN');stopPoll_();
  } finally{lock.releaseLock();}
}

