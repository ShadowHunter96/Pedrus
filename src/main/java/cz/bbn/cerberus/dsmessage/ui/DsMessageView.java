package cz.bbn.cerberus.dsmessage.ui;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.dsmessage.DsMessageService;
import cz.bbn.cerberus.dsmessage.dto.DsMessageFilterDto;
import cz.bbn.cerberus.dsmessage.dto.DsMessageSimpleDto;
import cz.bbn.cerberus.dsmessage.ui.component.DsMessageFilterComponent;
import cz.bbn.cerberus.dsmessage.ui.component.DsMessageGridComponent;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Sort;

import java.util.List;

@Route(value = DsMessageView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.DS_MESSAGE_VIEW)
@Slf4j
public class DsMessageView extends AppView {

    public static final String ROUTE = "ds-message-list";

    private final AppEnv appEnv;
    private final DsMessageService dsMessageService;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public DsMessageView(AppEnv appEnv, DsMessageService dsMessageService,
                         EntityNewComponentOperation entityNewComponentOperation) {
        this.appEnv = appEnv;
        this.dsMessageService = dsMessageService;
        this.entityNewComponentOperation = entityNewComponentOperation;
        initView();
    }

    private void initView() {
        AppCardGridComponent card = new AppCardGridComponent(Transl.get("DS message list"),
                entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY);
        card.setId(RobotFrameworkVariables.DS_MESSAGE_CARD_ID.getValue());
        card.setSizeFull();

        Button search = VaadinComponents.getSearchButton();
        DsMessageFilterComponent dsMessageFilterComponent = new DsMessageFilterComponent(
                search, getRecipientList(), getSenderNameList());
        card.add(dsMessageFilterComponent);

        DsMessageGridComponent dsMessageGridComponent =
                new DsMessageGridComponent(getDeleteAction(), appEnv, getItemsAction(dsMessageFilterComponent));

        dsMessageGridComponent.loadData();
        search.addClickListener(buttonClickEvent -> dsMessageGridComponent.loadData());

        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.BACKOFFICE));
        card.add(dsMessageGridComponent);
        this.add(card);
    }

    private List<String> getRecipientList() {
        return dsMessageService.getRecipientList();
    }

    private List<String> getSenderNameList() {
        return dsMessageService.getSenderNameList();
    }

    private ItemsAction<DsMessageSimpleDto> getItemsAction(DsMessageFilterComponent filterComponent) {
        return (query, orderList) -> {
            if (orderList.isEmpty()) {
                orderList.add(Sort.Order.desc("deliveryTime"));
            }
            DsMessageFilterDto filter = filterComponent.getDsMessageFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return dsMessageService.findDsMessageDtoPage(filter);
        };
    }

    private DeleteAction getDeleteAction() {
        return id -> {
            try {
                dsMessageService.deleteDsMessage(Long.valueOf(id));
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }
}
