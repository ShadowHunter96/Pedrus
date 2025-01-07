package cz.bbn.cerberus.activity;

import cz.bbn.cerberus.activity.dto.ActivityByObjectDto;
import cz.bbn.cerberus.activity.dto.ActivityByObjectFilterDto;
import cz.bbn.cerberus.activity.dto.ActivityLinkDto;
import cz.bbn.cerberus.activity.factory.ActivityByObjectFactory;
import cz.bbn.cerberus.activity.persistance.ActivityByObjectDao;
import cz.bbn.cerberus.activity.persistance.ActivityByObjectEntity;
import cz.bbn.cerberus.activity.persistance.ActivityByObjectId;
import cz.bbn.cerberus.activity.persistance.ActivityByObjectRepository;
import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;

@Service
public class ActivityByObjectService {

    private final ActivityByObjectDao activityByObjectDao;
    private final ActivityByObjectRepository activityByObjectRepository;
    private final AppLogService appLogService;

    public ActivityByObjectService(ActivityByObjectDao activityByObjectDao,
                                   ActivityByObjectRepository activityByObjectRepository,
                                   AppLogService appLogService) {
        this.activityByObjectDao = activityByObjectDao;
        this.activityByObjectRepository = activityByObjectRepository;
        this.appLogService = appLogService;
    }

    public Page<ActivityByObjectDto> findActivityByObjectEntityPage(ActivityByObjectFilterDto filterDto) {
        return activityByObjectDao.findEnumerationPage(filterDto);
    }

    public List<Long> getLinkedActivityIdList(String objectId, ObjectType objectType) {
        return activityByObjectRepository.getLinkedActivityIdList(objectId, objectType);
    }

    public List<ActivityByObjectDto> findAllAllowedList() {
        return ConvertEntities.fromEntities(
                activityByObjectRepository.findAllAllowed(), ActivityByObjectFactory::fromEntity);
    }

    @Transactional
    public void saveActivityByObject(ActivityLinkDto activityLinkDto) {
        activityLinkDto.getActivityDtoSet().forEach(enumerationDto -> {
            ActivityByObjectId activityByObjectId = new ActivityByObjectId();
            activityByObjectId.setObjectId(activityLinkDto.getObjectId());
            activityByObjectId.setObjectType(activityLinkDto.getObjectType());
            activityByObjectId.setActivityId(enumerationDto.getId());
            activityByObjectRepository.save(new ActivityByObjectEntity(activityByObjectId));
            appLogService.log("insert activity to object", "", activityByObjectId.toString());
        });
    }

    @Transactional
    public void deleteActivityByObject(ActivityByObjectId id) throws SystemException {
        if (!activityByObjectRepository.existsById(id)) {
            throw new SystemException(ErrorCode.TECHNOLOGY_OPPORTUNITY_NOT_EXITS);
        }
        activityByObjectRepository.deleteById(id);
        appLogService.log("delete activity from object", "", id.toString());
    }

}
