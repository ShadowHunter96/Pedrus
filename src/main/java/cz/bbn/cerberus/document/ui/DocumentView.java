package cz.bbn.cerberus.document.ui;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.component.ui.AppHelp;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.document.DocumentComponentOperation;
import cz.bbn.cerberus.document.ui.component.DocumentFilterComponent;
import cz.bbn.cerberus.document.ui.component.DocumentGridComponent;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Route(value = DocumentView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.DOCUMENT_TYPE_VIEW)
@Slf4j
public class DocumentView extends AppView {

    public static final String ROUTE = "document-list";

    private final DocumentComponentOperation documentComponentOperation;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public DocumentView(DocumentComponentOperation documentComponentOperation, AppEnv appEnv,
                        EntityNewComponentOperation entityNewComponentOperation) {
        this.documentComponentOperation = documentComponentOperation;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
        initView();
    }

    private void initView() {
        removeAll();
        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Document list"), entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY);

        AppHelp filterInfo = new AppHelp(this, Transl.get("Document filter info"), true);
        filterInfo.setContent(getDocumentInfoLayout());

        Button search = VaadinComponents.getSearchButton();
        DocumentFilterComponent documentFilterComponent =
                new DocumentFilterComponent(documentComponentOperation.getDocumentTypeDtoList(), search, filterInfo);
        card.add(documentFilterComponent);
        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation));

        DocumentGridComponent grid = new DocumentGridComponent(appEnv, documentComponentOperation,
                documentComponentOperation.getItemsAction(documentFilterComponent),
                documentComponentOperation.getListAction(), Permission.DOCUMENT_DOWNLOAD, Permission.DOCUMENT_EDIT,
                documentComponentOperation.getLinkAction(),
                documentComponentOperation.getItemActionDocumentFile(),
                documentComponentOperation.getDeleteOrRestoreAction(),
                documentComponentOperation.getDocumentTypeDtoList());
        card.add(grid);

        add(card);
        grid.loadData();
        search.addClickListener(buttonClickEvent -> grid.loadData());
    }

    private VerticalLayout getDocumentInfoLayout() {
        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.add(new Label(Transl.get("Name - fulltext search of document name")));
        verticalLayout.add(new Label(Transl.get("Type - fulltext search of document prefix")));
        verticalLayout.add(new Label(Transl.get("Document type - fulltext search of document type")));
        verticalLayout.add(new Label(Transl.get(
                "Object type - you can choose one of this - project, contact person, subject, contract")));
        verticalLayout.add(new Label(Transl.get(
                "Object id - specific id according to the selected object type - fulltext search")));
        verticalLayout.add(new Label(Transl.get(
                "Show only unlinked - show documents that are not connected to any other object")));
        verticalLayout.add(new Label(Transl.get(
                "Show only deleted - shows documents that are ready for complete deletion")));
        return verticalLayout;
    }
}
