package cz.bbn.cerberus.note;

import com.vaadin.flow.component.checkbox.Checkbox;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.GetItemAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.component.ui.slidetab.CountIntIndicator;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.note.dto.NoteDto;
import cz.bbn.cerberus.note.dto.NoteFilterDto;
import cz.bbn.cerberus.note.dto.NoteObjectIdNameDto;
import cz.bbn.cerberus.note.ui.component.NoteFilterComponent;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Service
@Slf4j
public class NoteComponentOperation {

    private final NoteService noteService;
    private final AppEnv appEnv;
    private final ListService listService;

    public NoteComponentOperation(NoteService noteService, ListService listService, AppEnv appEnv) {
        this.noteService = noteService;
        this.listService = listService;
        this.appEnv = appEnv;
    }

    public SaveAction<NoteDto> getNoteDtoSaveAction(String id, NoteTypeEnum noteTypeEnum) {
        return (dto, originalDto) -> {
            try {
                if (dto.getId() != null) {
                    noteService.updateNote(dto, originalDto);
                } else {
                    dto.setType(noteTypeEnum);
                    dto.setEntityId(id);
                    dto.setUserDto(SecurityUtils.getCurrentUserDto());
                    dto.setDate(LocalDateTime.now());
                    noteService.saveNote(dto);
                }
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public SaveAction<NoteDto> getNoteDtoSaveAction() {
        return (dto, originalDto) -> {
            try {
                if (dto.getId() != null) {
                    noteService.updateNote(dto, originalDto);
                    SuccessNotification.showSavingSuccess(appEnv);
                }
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public ItemsAction<NoteDto> getItemsAction(String id, Checkbox showArchived, NoteTypeEnum noteTypeEnum,
                                               NoteFilterComponent filterComponent) {
        return (query, orderList) -> {
            NoteFilterDto filter = filterComponent.getFilter();
            filter.setNoteTypeEnum(noteTypeEnum);
            filter.setEntityId(id);
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setShowArchived(showArchived.getValue());
            return noteService.findNoteDtoPage(filter, orderList);
        };
    }

    public ItemsAction<NoteDto> getItemsAction(NoteFilterComponent noteFilterComponent) {
        return (query, orderList) -> {
            NoteFilterDto filter = noteFilterComponent.getFilter();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            return noteService.findNoteDtoPage(filter, orderList);
        };
    }

    public GetItemAction<NoteDto> getItemAction() {
        return id -> {
            try {
                return noteService.getNote(Long.valueOf(id));
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
            return null;
        };
    }

    public DeleteAction getDeleteAction() {
        return id -> {
            try {
                noteService.deleteNote(Long.valueOf(id));
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    public DeleteAction getDeleteAction(CountIntIndicator indicator, NoteTypeEnum type, String objectId) {
        return id -> {
            try {
                noteService.deleteNote(Long.valueOf(id));
                if (indicator != null) {
                    indicator.setCount(noteService.getNoteCountByTypeAndObjectId(type, objectId));
                }
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    public List<UserDto> getUserListForObject(String entityId, NoteTypeEnum noteTypeEnum) {
        Set<UserEntity> userEntitySet = noteService.findNoteUsers(entityId, noteTypeEnum);
        List<UserEntity> userEntityList = userEntitySet.stream().toList();
        List<UserDto> userDtoList = ConvertEntities.fromEntities(userEntityList, UserFactory::fromEntity);
        userDtoList.sort(Comparator.comparing(UserDto::getName));
        return userDtoList;
    }

    public List<NoteObjectIdNameDto> getNoteObjectIdNameDtoList(String objectType) {
        List<NoteObjectIdNameDto> noteObjectIdNameDtoList = new ArrayList<>();
        Set<String> idSet = new HashSet<>(SecurityUtils.getCustomReadPermission(objectType));

        for (String id : idSet) {
            NoteObjectIdNameDto dto = new NoteObjectIdNameDto();
            dto.setId(id);
            dto.setName(listService.getNameByIdAndObjectType(id, objectType));
            noteObjectIdNameDtoList.add(dto);
        }

        noteObjectIdNameDtoList.sort(Comparator.comparing(NoteObjectIdNameDto::getName));
        return noteObjectIdNameDtoList;
    }

    public List<NoteTypeEnum> getNoteTypeEnumList() {
        List<NoteTypeEnum> noteTypeEnumList = new ArrayList<>();
        for (NoteTypeEnum noteTypeEnum : NoteTypeEnum.values()) {
            if (NoteTypeEnum.ANY != noteTypeEnum) {
                noteTypeEnumList.add(noteTypeEnum);
            }
        }
        noteTypeEnumList.sort(Comparator.comparing(NoteTypeEnum::getObjectName));
        return noteTypeEnumList;
    }

    public int getNoteCountByTypeAndObjectId(NoteTypeEnum type, String objectId) {
        return noteService.getNoteCountByTypeAndObjectId(type, objectId);
    }

    public int getNoteCountByType(NoteFilterDto filter) {
        return noteService.getNoteCountByType(filter);
    }

    public ListService getListService() {
        return listService;
    }
}
