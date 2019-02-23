pid = Sys.getpid()
thread_log_file <<- paste('thread_log',pid,Sys.time(),sep='-')
aprint <<- function(msg) { write(msg,file=thread_log_file,append=TRUE) } 
aprint('Thread started')

while(TRUE){
  aprint(paste('Going through files..',Sys.time()))
  new_files = grep('new_task',list.files())
  if(length(new_files)>0){
    f = list.files()[new_files[1]]
    f_new = gsub('new_task','task',f)
    file.rename(f, f_new)
    sched_log_file <<- paste('task_log',pid,Sys.time(),sep='-')
    bprint <<- function(msg) { write(msg,file=sched_log_file,append=TRUE) } 
    bprint('Starting task')
    aprint('Starting task..')
    source(f_new)
  }

  Sys.sleep(20)
}

