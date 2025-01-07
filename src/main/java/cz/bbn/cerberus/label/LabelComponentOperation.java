package cz.bbn.cerberus.label;

import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.provider.ListDataProvider;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.label.dto.LabelDto;
import cz.bbn.cerberus.label.dto.LabelFilterDto;
import cz.bbn.cerberus.label.dto.LabelType;
import cz.bbn.cerberus.label.ui.LabelDetailView;
import cz.bbn.cerberus.label.ui.LabelView;
import cz.bbn.cerberus.label.ui.component.LabelFilterComponent;
import cz.bbn.cerberus.label.ui.component.LabelGridComponent;
import cz.bbn.cerberus.label.ui.component.LabelNewDialog;
import cz.bbn.cerberus.labelsubject.persistance.LabelSubjectRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;


@Component
@Slf4j
public class LabelComponentOperation {

    private final LabelService labelService;
    private final AppEnv appEnv;
    private final LabelSubjectRepository labelSubjectRepository;

    public LabelComponentOperation(LabelService labelService, AppEnv appEnv,
                                   LabelSubjectRepository labelSubjectRepository) {
        this.labelService = labelService;
        this.appEnv = appEnv;
        this.labelSubjectRepository = labelSubjectRepository;
    }

    public ItemsAction<LabelDto> getItemsAction(LabelFilterComponent filterComponent) {
        return (query, orderList) -> {
            LabelFilterDto filter = filterComponent.getLabelFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return labelService.findLabelDtoPage(filter);
        };
    }

    public SaveAction<LabelDto> getSaveAction(AppDialog appDialog, VerticalLayout layout) {
        return (newDto, originalDto) -> {
            newDto.setTableValueList(getValuesFromTable(newDto, layout));
            try {
                if (originalDto.getId() != null) {
                    labelService.updateLabel(newDto, originalDto);
                    UI.getCurrent().navigate(LabelView.ROUTE);
                } else {
                    newDto.setDeleted(Boolean.FALSE);
                    labelService.saveLabel(newDto);
                    if (appDialog != null) {
                        appDialog.close();
                        UI.getCurrent().navigate(LabelDetailView.ROUTE.concat("/").concat(newDto.getId()));
                    }
                }
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public List<ItemDto> getValuesFromTable(LabelDto labelDto, VerticalLayout layout) {
        if (labelDto.getType() == LabelType.TABLE) {
            Grid<ItemDto> grid = (Grid<ItemDto>) layout.getComponentAt(0);
            return (List<ItemDto>) ((ListDataProvider<ItemDto>) grid.getDataProvider()).getItems();
        } else {
            return null;
        }
    }

    public ListAction<String> getListAction() {
        return labelSubjectRepository::findByLabelId;
    }

    public ComponentEventListener<
            ClickEvent<? extends com.vaadin.flow.component.Component>
            > getNewLabelEvent(LabelGridComponent grid) {
        return buttonClickEvent -> {
            LabelNewDialog newDialog = new LabelNewDialog(grid, this, appEnv);
            newDialog.open();
        };
    }
}
