using System;
using System.IO;
using System.Threading.Tasks;
using Avalonia.Controls;
using Avalonia.Interactivity;
using Avalonia.Markup.Xaml;
using Avalonia.Threading;
using static Interpreter;

namespace Gui.Views
{
    public class MainWindow : Window
    {
        private readonly TextBox _codeInput;
        private readonly TextBlock _console;
        private readonly TextBlock _statusBar;
        private readonly MenuItem _run;

        public MainWindow()
        {
            InitializeComponent();
            _codeInput = this.Find<TextBox>( "CodeInput");
            _console = this.Find<TextBlock>( "Console");
            _run = this.FindControl<MenuItem>("Run");
            _statusBar = this.FindControl<TextBlock>("StatusBar");
        }

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }

        public async void Open(object? sender, RoutedEventArgs e)
        {
            var dialog = new OpenFileDialog();
            dialog.Filters.Add(new FileDialogFilter { Extensions = { "txt" } });
            var result = await dialog.ShowAsync(this);
            if (result is {Length: > 0})
            {
                _codeInput.Text = await File.ReadAllTextAsync(result[0]);
            }
        }

        public async void Save(object? sender, RoutedEventArgs e)
        {
            var dialog = new SaveFileDialog();
            var result = await dialog.ShowAsync(this);
            if (result != null)
            {
                await File.WriteAllTextAsync(result, _codeInput.Text);
            }
        }

        public void New(object? sender, RoutedEventArgs e)
        {
            _codeInput.Text = "";
            Save(sender, e);
        }

        private void Run(object? sender, RoutedEventArgs routedEventArgs)
        {
            _run.IsEnabled = false;
            var resStr = "";
            var task = new Task<(string, string)>(() =>
            {
                try
                {
                    if (_codeInput.Text == null) return ("Empty input, nothing to compute", resStr);
                    var (_, _, pDictionary) = run(parse(_codeInput.Text));
                    var k = pDictionary[outputBuffer];
                    resStr += k;
                    return ("Computed successfully!", resStr);
                }
                catch (Exception ex)
                {
                    return (ex.Message, resStr);
                }
            });
            task.ContinueWith(x => 
                Dispatcher.UIThread.Post(() =>
                {
                    var (status, result) = x.Result;
                    _statusBar.Text = status;
                    _console.Text = result;
                    _run.IsEnabled = true;
                })
            );
            task.Start();
        }
    }
}
