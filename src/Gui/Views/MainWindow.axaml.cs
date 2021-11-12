using System;
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
        public static TextBox CodeInput = null!;
        private readonly TextBlock _console;
        public static TextBlock StatusBar = null!;
        public static string? Filename;

        public MainWindow()
        {
            InitializeComponent();
            CodeInput = this.Find<TextBox>( "CodeInput");
            _console = this.Find<TextBlock>( "Console");
            StatusBar = this.FindControl<TextBlock>("StatusBar");
            Filename = "";
        }

        public static readonly MainWindow main = new ();

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }

        private void Run(object? sender, RoutedEventArgs routedEventArgs)
        {
            if (string.IsNullOrEmpty(CodeInput.Text))
            {
                StatusBar.Text = "Empty entry, nothing to compute";
                return;
            }
            StatusBar.Text = "Computing";
            var task = new Task(() =>
            {
                try
                {
                    var (_, _, pd) = run(parse(CodeInput.Text));
                    Dispatcher.UIThread.Post(() =>
                    {
                        _console.Text = pd[outputBuffer];
                        StatusBar.Text = "Computed successfully";
                    });
                }
                catch (Exception ex)
                {
                    Dispatcher.UIThread.Post(() =>
                    {
                        StatusBar.Text = ex.Message;
                    });
                }
            });
            task.Start();
        }
    }
}
