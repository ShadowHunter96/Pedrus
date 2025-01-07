package cz.bbn.cerberus.permissionmanagement.ui.component;

import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import cz.bbn.cerberus.commons.component.ui.interfaces.ChangePageAction;

import java.util.ArrayList;
import java.util.List;

public class PageChangeComponent extends HorizontalLayout {

    private final ChangePageAction changePageAction;

    private int currentPage;
    private int noOfPages;

    public PageChangeComponent(ChangePageAction changePageAction) {
        this.changePageAction = changePageAction;
        setWidthFull();
    }

    public void generateComponent(int currentPage, int noOfPages) {
        this.currentPage = currentPage;
        this.noOfPages = noOfPages;
        this.setJustifyContentMode(JustifyContentMode.CENTER);
        this.setDefaultVerticalComponentAlignment(Alignment.CENTER);
        generatePageLine();
    }


    private void generatePageLine() {
        removeAll();
        Span first = new Span("<<");
        Span previous = new Span("<");
        Span next = new Span(">");
        Span last = new Span(">>");
        first.addClassName("cursor-pointer");
        first.getElement().getStyle().set("padding-right", "0.3em").set("padding-left", "0.3em");
        previous.addClassName("cursor-pointer");
        previous.getElement().getStyle().set("padding-right", "0.3em").set("padding-left", "0.3em");
        next.addClassName("cursor-pointer");
        next.getElement().getStyle().set("padding-right", "0.3em").set("padding-left", "0.3em");
        last.addClassName("cursor-pointer");
        last.getElement().getStyle().set("padding-right", "0.3em").set("padding-left", "0.3em");
        if (currentPage != 1) {
            first.addClickListener(e -> {
                changePageAction.changePage(1);
                generateComponent(1, noOfPages);
            });
            previous.addClickListener(e -> {
                int prevPage = currentPage - 1;
                changePageAction.changePage(prevPage);
                generateComponent(prevPage, noOfPages);
            });
        }
        add(first, previous);
        List<Integer> pageList = new ArrayList<>();
        if (currentPage > 3) {
            add(new Span("..."));
        }

        for (int i = 1; i <= noOfPages; i++) {
            pageList.add(i);
            if (i > currentPage - 3 && i < currentPage + 3) {
                int finalI = i;
                Span span = new Span("" + i);
                span.addClassName("cursor-pointer");
                if (i == currentPage) {
                    span.getElement().getStyle().set("color", "var(--lumo-primary-color)");
                }
                add(span);
                span.addClickListener(e -> {
                    changePageAction.changePage(finalI);
                    generateComponent(finalI, noOfPages);
                });
            }
        }
        if (currentPage < noOfPages - 2) {
            add(new Span("..."));
        }
        if (currentPage != noOfPages) {
            next.addClickListener(e -> {
                int nextPage = currentPage + 1;
                changePageAction.changePage(nextPage);
                generateComponent(nextPage, noOfPages);
            });
            last.addClickListener(e -> {
                changePageAction.changePage(noOfPages);
                generateComponent(noOfPages, noOfPages);
            });
        }
        add(next, last);
        ComboBox<Integer> selectPage = new ComboBox<>();
        selectPage.setWidth("6em");
        selectPage.setItems(pageList);
        selectPage.setValue(currentPage);
        selectPage.addValueChangeListener(e -> {
            if (e.getValue() != null && e.getValue() > 0 && e.getValue() < noOfPages + 1) {
                changePageAction.changePage(e.getValue());
                generateComponent(e.getValue(), noOfPages);
            }
        });
        add(selectPage);
    }


}
