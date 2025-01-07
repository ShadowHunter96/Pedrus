package cz.bbn.cerberus.applog;

import cz.bbn.cerberus.applog.dto.AppLogDto;
import cz.bbn.cerberus.applog.dto.AppLogFilterDto;
import cz.bbn.cerberus.applog.persistance.AppLogDao;
import cz.bbn.cerberus.applog.persistance.AppLogEntity;
import cz.bbn.cerberus.applog.persistance.AppLogRepository;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.DiffBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Service
@Slf4j
public class AppLogService {

    private final AppLogRepository appLogRepository;
    private final AppLogDao appLogDao;

    public AppLogService(AppLogRepository appLogRepository, AppLogDao appLogDao) {
        this.appLogRepository = appLogRepository;
        this.appLogDao = appLogDao;
    }

    public Page<AppLogDto> findAppLogDtoPage(AppLogFilterDto filter) {
        return appLogDao.findAppLogDtoPage(filter);
    }

    public List<ItemDto> getActionList() {
        List<ItemDto> resultList = new ArrayList<>();
        List<String> list = appLogRepository.getActionList();
        list.forEach(s -> resultList.add(new ItemDto(s, s)));
        return resultList;
    }

    @Transactional
    public void logLogin(Long userId, String message) {
        log("login", message, "", userId);
    }

    @Transactional
    public void logUpdate(Object dto, Object originalDto, String objectName) {
        String diff = getObjectsDiff(dto, originalDto);
        Method method = ReflectionUtils.findMethod(dto.getClass(), "getId");
        String id = method != null ? String.valueOf(ReflectionUtils.invokeMethod(method, dto)) : "";

        log(objectName.concat(" update"), diff, id);
    }

    @Transactional
    public void logInsert(Object dto, String objectName) {
        Method method = ReflectionUtils.findMethod(dto.getClass(), "getId");
        String id = method != null ? String.valueOf(ReflectionUtils.invokeMethod(method, dto)) : "";
        log(objectName.concat(" insert"), dto.toString(), id);
    }

    @Transactional
    public void logInsertMultiple(Set<?> dtoSet, String objectName) {
        Set<AppLogEntity> entitySet = new HashSet<>();
        for (Object dto : dtoSet) {
            Method method = ReflectionUtils.findMethod(dto.getClass(), "getId");
            String id = method != null ? String.valueOf(ReflectionUtils.invokeMethod(method, dto)) : "";
            entitySet.add(convertToEntity(
                    objectName.concat(" insert"), dto.toString(), id, SecurityUtils.getCurrentUserId()));
        }
        logMultiple(entitySet);
    }

    @Transactional
    public void logDeleteMultiple(Set<?> dtoSet, String objectName) {
        Set<AppLogEntity> entitySet = new HashSet<>();
        for (Object dto : dtoSet) {
            Method method = ReflectionUtils.findMethod(dto.getClass(), "getId");
            String id = method != null ? String.valueOf(ReflectionUtils.invokeMethod(method, dto)) : "";
            entitySet.add(convertToEntity(
                    objectName.concat(" delete"), dto.toString(), id, SecurityUtils.getCurrentUserId()));
        }
        logMultiple(entitySet);
    }

    @Transactional
    public void logDelete(String id, String objectName) {
        log(objectName.concat(" delete"), "", id);
    }


    @Transactional
    public void log(String action, String message, String id) {
        log(action, message, id, SecurityUtils.getCurrentUserId());
    }

    @Transactional
    public void log(String action, String message, Long userId) {
        log(action, message, "", userId);
    }

    public void log(String action, String message, String id, Long userId) {
        AppLogEntity appLogEntity = convertToEntity(action, message, id, userId);
        appLogRepository.save(appLogEntity);
    }

    private void logMultiple(Set<AppLogEntity> entitySet) {
        appLogRepository.saveAll(entitySet);
    }

    private AppLogEntity convertToEntity(String action, String message, String id, Long userId) {
        AppLogEntity appLogEntity = new AppLogEntity();
        appLogEntity.setUserId(userId);
        appLogEntity.setAppId(id);
        appLogEntity.setAction(action);
        appLogEntity.setDate(LocalDateTime.now());
        appLogEntity.setMessage(StringUtils.trimToEmpty(message).replace("\u0000", ""));
        return appLogEntity;
    }

    private String getObjectsDiff(Object dto, Object originalDto) {
        DiffBuilder diffBuilder = new DiffBuilder(dto, originalDto, ToStringStyle.SHORT_PREFIX_STYLE);
        try {
            for (Field field : dto.getClass().getDeclaredFields()) {
                field.setAccessible(true);
                Object instance = field.get(dto);
                Object originalInstance = field.get(originalDto);
                diffBuilder.append(field.getName(), originalInstance, instance);
            }
        } catch (IllegalAccessException e) {
            log.error("Parse to Administration log error", e);
        }
        return diffBuilder.build().toString();
    }
}
