package cz.bbn.cerberus.translation;

import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.interfaces.ConfirmAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.translation.dto.TranslationDto;
import cz.bbn.cerberus.translation.dto.TranslationFilterDto;
import cz.bbn.cerberus.translation.ui.component.TranslationFilterComponent;
import cz.bbn.cerberus.translation.ui.component.TranslationGrid;
import lombok.extern.log4j.Log4j;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
@Log4j
public class TranslationComponentOperation {

    private final TranslationService translationService;
    private final AppEnv appEnv;

    public TranslationComponentOperation(TranslationService translationService, AppEnv appEnv) {
        this.translationService = translationService;
        this.appEnv = appEnv;
    }

    public DeleteAction getDeleteAction() {
        return id -> {
            try {
                translationService.deleteTranslation(Long.valueOf(id));
            } catch (SystemException e) {
                log.error(e.getMessage(), e);
                ErrorNotification.show(e.getMessage(), appEnv);
            }
        };
    }

    public ItemsAction<TranslationDto> getItemsAction(TranslationFilterComponent filterComponent) {
        return (query, orderList) -> {
            TranslationFilterDto translationFilterDto = filterComponent.getTranslationFilterDto();
            translationFilterDto.setPage(query.getPage());
            translationFilterDto.setSize(query.getPageSize());
            translationFilterDto.setOrderList(orderList);
            return translationService.getTranslationPage(translationFilterDto);
        };
    }

    public List<String> getLangSet() {
        List<String> langList = new ArrayList<>();
        langList.add("cs");
        langList.add("en");
        return langList;
    }

    public TranslationDto getCsDto(TranslationDto item) {
        if (("cs").equalsIgnoreCase(item.getLang())) {
            item.setLang("cs");
            return item;
        }
        return translationService.getTranslationDto("cs", item.getKey());
    }

    public TranslationDto getEnDto(TranslationDto item) {
        if (("en").equalsIgnoreCase(item.getLang())) {
            item.setLang("en");
            return item;
        }
        return translationService.getTranslationDto("en", item.getKey());
    }

    public void saveCsEnDto(TranslationDto csDto, TranslationDto enDto) {
        translationService.save(csDto);
        translationService.save(enDto);
    }

    public ConfirmAction getDeleteConfirmAction(TranslationDto dto, TranslationGrid translationGrid) {
        return () -> delete(dto, translationGrid);
    }

    public void delete(TranslationDto dto, AppInfiniteGrid<TranslationDto> grid) {
        try {
            translationService.deleteTranslation(dto.getId());
            grid.loadData();
            SuccessNotification.showSavingSuccess(appEnv);
        } catch (SystemException e) {
            log.error(TextValues.SYSTEM_EXCEPTION, e);
            ErrorNotification.show(e.getMessage(), appEnv);
        }
    }
}
