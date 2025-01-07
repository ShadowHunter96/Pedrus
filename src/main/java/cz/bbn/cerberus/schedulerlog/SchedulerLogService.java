package cz.bbn.cerberus.schedulerlog;


import cz.bbn.cerberus.schedulerlog.dto.SchedulerLogDto;
import cz.bbn.cerberus.schedulerlog.dto.SchedulerLogFilterDto;
import cz.bbn.cerberus.schedulerlog.persistance.SchedulerLogDao;
import cz.bbn.cerberus.schedulerlog.persistance.SchedulerLogRepository;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class SchedulerLogService {

    private final SchedulerLogDao schedulerLogDao;
    private final SchedulerLogRepository schedulerLogRepository;

    public SchedulerLogService(SchedulerLogDao schedulerLogDao,
                               SchedulerLogRepository schedulerLogRepository) {
        this.schedulerLogDao = schedulerLogDao;
        this.schedulerLogRepository = schedulerLogRepository;
    }

    public Page<SchedulerLogDto> findSchedulerLogDtoPage(SchedulerLogFilterDto filter) {
        return schedulerLogDao.findSchedulerLogPage(filter);
    }

    public List<String> getDescriptionList(){
        return schedulerLogRepository.getDescriptionList();
    }

}
