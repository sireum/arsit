#include <all.h>
#include <sys/types.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <unistd.h>

static inline void sem_op(int sid, short val) {
    struct sembuf sem_op;
    sem_op.sem_num = 0;
    sem_op.sem_op = val;
    sem_op.sem_flg = 0;
    semop(sid, &sem_op, 1);
}

static inline void lock(int sid) {
    sem_op(sid, -1);
}

static inline void unlock(int sid) {
    sem_op(sid, 1);
}

static inline int create_sem(Z msgid) {
    unsigned int permission = 0666;
    unsigned int mask = IPC_CREAT;
    int sem_set_id = semget((key_t) msgid, 1, mask | permission);

    if (sem_set_id >= 0) {
        union semun {
            int val;
            struct semid_ds *buf;
            ushort *array;
        } sem_val;
        sem_val.val = 1;
        semctl(sem_set_id, 0, SETVAL, sem_val);
    }
    return sem_set_id;
}

Z PACKAGE_NAME_SharedMemory_create(StackFrame caller, Z id) {
    unsigned int permission = 0666;
    unsigned int mask = IPC_CREAT;

    create_sem(id);

    int shmid = shmget((key_t) id, sizeof(union Option_8E9F45), (int) (permission | mask));
    void *p = shmat(shmid, (void *) 0, 0);
    memset(p, 0, sizeof(union Option_8E9F45));
    shmdt(p);

    return (Z) shmid;
}

void PACKAGE_NAME_SharedMemory_receive(art_DataContent result, StackFrame caller, Z port) {
    int sid = semget((key_t) port, 1, 0666);

    lock(sid);

    int shmid = shmget((key_t) port, sizeof(union Option_8E9F45), 0666);

    Option_8E9F45 p = (Option_8E9F45) shmat(shmid, (void *) 0, 0);

    while (p->type != TSome_D29615) { // wait until there is a data
        unlock(sid);
        usleep((useconds_t) 10 * 1000);
        lock(sid);
    }

    art_DataContent d = &p->Some_D29615.value;
    Type_assign(result, d, sizeOf((Type) d));
    memset(p, 0, sizeof(union Option_8E9F45));
    shmdt(p);

    unlock(sid);
}

void PACKAGE_NAME_SharedMemory_receiveAsync(Option_8E9F45 result, StackFrame caller, Z port) {
    int sid = semget((key_t) port, 1, 0666);

    lock(sid);

    int shmid = shmget((key_t) port, sizeof(union Option_8E9F45), 0666);

    Option_8E9F45 p = (Option_8E9F45) shmat(shmid, (void *) 0, 0);

    if (p->type == TSome_D29615) {
        Type_assign(result, p, sizeOf((Type) p));
        memset(p, 0, sizeof(union Option_8E9F45));
    } else {
        result->type = TNone_964667;
    }

    shmdt(p);

    unlock(sid);
}

Unit PACKAGE_NAME_SharedMemory_send(StackFrame caller, Z destid, Z port, art_DataContent d) {
    int sid = semget((key_t) port, 1, 0666);

    lock(sid);

    int shmid = shmget((key_t) destid, sizeof(union Option_8E9F45), 0666);

    Option_8E9F45 p = (Option_8E9F45) shmat(shmid, (void *) 0, 0);

    while (p->type == TSome_D29615) {
        unlock(sid);
        usleep((useconds_t) 10 * 1000);
        lock(sid);
    }

    p->type = TSome_D29615;
    Type_assign(&(p->Some_D29615.value), d, sizeOf((Type) d));

    shmdt(p);

    unlock(sid);
}

B PACKAGE_NAME_SharedMemory_sendAsync(StackFrame caller, Z destid, Z port, art_DataContent d) {
    int sid = semget((key_t) port, 1, 0666);

    lock(sid);

    int shmid = shmget((key_t) port, sizeof(union Option_8E9F45), 0666);

    Option_8E9F45 p = (Option_8E9F45) shmat(shmid, (void *) 0, 0);
    p->type = TSome_D29615;
    Type_assign(&(p->Some_D29615.value), d, sizeOf((Type) d));

    shmdt(p);

    unlock(sid);
    return T;
}

Unit PACKAGE_NAME_SharedMemory_remove(StackFrame caller, Z id) {
    semctl(semget((key_t) id, 1, 0666), 0, IPC_RMID);
    shmctl(shmget((key_t) id, sizeof(union Option_8E9F45), 0666), IPC_RMID, NULL);
}

Unit PACKAGE_NAME_Process_sleep(StackFrame caller, Z n) {
    usleep((useconds_t) n * 1000);
}
